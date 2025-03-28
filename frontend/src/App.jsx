import { AuthClient } from "@dfinity/auth-client";
import { createActor } from "declarations/backend";
import { canisterId } from "declarations/backend/index.js";
import React, { useState, useEffect } from "react";
import "../index.css";
import ConcertDetailsModal from "./ConcertDetailsModal";
import CustomerDashboard from "./CustomerDashboard";
import OrganizerDashboard from "./OrganizerDashboard";
import AdminDashboard from "./AdminDashboard";
import ConcertCard from "./ConcertCard";

const network = process.env.DFX_NETWORK || "local";
const identityProvider = network === "ic" ? "https://identity.ic0.app" : "http://rdmx6-jaaaa-aaaaa-aaadq-cai.localhost:4943";

function App() {
  const [isAuthenticated, setIsAuthenticated] = useState(false);
  const [authClient, setAuthClient] = useState(null);
  const [actor, setActor] = useState(null);
  const [role, setRole] = useState(null);
  const [principal, setPrincipal] = useState(null);
  const [selectedRole, setSelectedRole] = useState("");
  const [name, setName] = useState("");
  const [concerts, setConcerts] = useState([]);
  const [selectedConcert, setSelectedConcert] = useState(null);
  const [search, setSearch] = useState("");
  const [minPrice, setMinPrice] = useState("");
  const [maxPrice, setMaxPrice] = useState("");
  const [onlyAvailable, setOnlyAvailable] = useState(false);
  const [isLoading, setIsLoading] = useState(false);
  const [tokenInitialized, setTokenInitialized] = useState(false);
  const [tickets, setTickets] = useState([]);
  const [ticketConcerts, setTicketConcerts] = useState([]);
  const [balance, setBalance] = useState(0);

  useEffect(() => { 
    initializeAuth(); 
  }, []);

  useEffect(() => { 
    if (isAuthenticated && role) { 
      checkTokenInitialization(); 
      if (tokenInitialized) fetchConcerts(); 
    } 
  }, [isAuthenticated, role, search, minPrice, maxPrice, onlyAvailable, tokenInitialized]);

  async function initializeAuth() {
    try {
      const client = await AuthClient.create();
      const isAuth = await client.isAuthenticated();
      setAuthClient(client);
      if (isAuth) {
        const identity = client.getIdentity();
        const principal = identity.getPrincipal();
        const newActor = createActor(canisterId, { agentOptions: { identity } });
        setActor(newActor);
        setPrincipal(principal);
        setIsAuthenticated(true);
        const userRole = await newActor.getRole();
        if (userRole) setRole(userRole);
      }
    } catch (error) {
      console.error("Initialization failed:", error);
    }
  }

  async function checkTokenInitialization() {
    try {
      const initialized = await actor.isTokenInitialized();
      setTokenInitialized(initialized);
    } catch (error) {
      console.error("Failed to check token initialization:", error);
      setTokenInitialized(false);
    }
  }

  async function login() {
    try {
      await authClient.login({
        identityProvider,
        onSuccess: async () => {
          const identity = authClient.getIdentity();
          const principal = identity.getPrincipal();
          const newActor = createActor(canisterId, { agentOptions: { identity } });
          setActor(newActor);
          setPrincipal(principal);
          setIsAuthenticated(true);
          const userRole = await newActor.getRole();
          if (userRole) setRole(userRole);
        },
      });
    } catch (error) {
      console.error("Login failed:", error);
      alert("Login failed. Please try again.");
    }
  }

  async function logout() {
    try {
      await authClient.logout();
      setIsAuthenticated(false);
      setActor(null);
      setPrincipal(null);
      setRole(null);
      setConcerts([]);
      setSelectedConcert(null);
      setTokenInitialized(false);
    } catch (error) {
      console.error("Logout failed:", error);
    }
  }

  async function registerRole() {
    if (!selectedRole) { alert("Please select a role"); return; }
    if (!name.trim()) { alert("Please enter a name"); return; }
    try {
      const result = await actor.register(selectedRole, name);
      if (result.includes("Error")) alert(result);
      else setRole(selectedRole);
    } catch (error) {
      console.error("Registration failed:", error);
      alert(`Failed to register: ${error.message}`);
    }
  }

  async function fetchConcerts() {
    setIsLoading(true);
    try {
      const searchArg = search.trim() || "";
      const minPriceArg = minPrice ? parseInt(minPrice) : 0;
      const maxPriceArg = maxPrice ? parseInt(maxPrice) : 0;
      const concertsList = await actor.getConcerts(searchArg, minPriceArg, maxPriceArg, onlyAvailable);
      setConcerts(concertsList);
    } catch (error) {
      console.error("Failed to fetch concerts:", error);
      setConcerts([]);
    } finally {
      setIsLoading(false);
    }
  }

  async function handleBuyTicket(concertId) {
    try {
      const ticketId = await actor.buyTicket(concertId);
      alert(`${ticketId}`);
      fetchConcerts();
      await fetchTickets();
      await fetchCustomerBalance();
    } catch (error) {
      alert(`Failed to buy ticket: ${error.message}`);
    }
  }

  const fetchTickets = async () => {
    try {
      if (!principal) return;
      const userTickets = await actor.getCustomerTickets(principal);
      setTickets(userTickets);

      const concertPromises = userTickets.map(async (ticket) => {
        const [concert, found] = await actor.getConcert(ticket.concertId);
        return found ? concert : null;
      });

      const concertResults = await Promise.all(concertPromises);
      setTicketConcerts(concertResults.filter((c) => c !== null));
    } catch (err) {
      console.error("Error fetching tickets:", err);
      setTickets([]);
      setTicketConcerts([]);
    }
  };

  const fetchCustomerBalance = async () => {
    try {
      if (!principal) {
        console.error("Principal is not defined");
        return;
      }
      const bal = await actor.getCustomerBalance(principal);
      setBalance(Number(bal) / 100_000_000);
    } catch (error) {
      console.error("Failed to fetch customer balance:", error);
      setBalance(0);
    }
  };
  
  return (
    <div className="min-h-screen bg-gray-100">
      {!isAuthenticated ? (
        <div className="relative min-h-screen bg-gray-100">
          <div className="flex flex-col items-center justify-center min-h-[calc(100vh-80px)] text-center px-4">
            <h1 className="text-5xl md:text-6xl font-extrabold text-gray-800 mb-4 tracking-tight drop-shadow-md animate-fade-in-down">Welcome to <span className="text-blue-500">Medley</span></h1>
            <div className="p-2 flex items-center justify-center">
              <img src="https://res.cloudinary.com/dqvlnzw9f/image/upload/f_auto,q_auto/v1/Medley/ci3v3mkgpkhxjjfr7akc" alt="Medley Logo" className="h-64 w-auto" />
            </div>
            <p className="text-lg md:text-xl text-gray-600 max-w-3xl mb-8 leading-relaxed drop-shadow-md animate-fade-in-up">Discover a new era of concert events with Medley—a decentralized platform powered by blockchain technology. Securely manage tickets with NFTs, ensuring transparency and fairness for organizers and fans alike.</p>
            <button onClick={login} className="flex items-center px-6 py-3 rounded-lg text-lg font-medium bg-blue-500 text-white hover:bg-blue-600 transition-all duration-200 shadow-lg">
              <svg className="w-5 h-5 mr-2" fill="none" stroke="currentColor" viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg"><path strokeLinecap="round" strokeLinejoin="round" strokeWidth="2" d="M11 16l-4-4m0 0l4-4m-4 4h14" /></svg>
              Get Started with Internet Identity
            </button>
          </div>
          <footer className="absolute bottom-0 w-full p-4 text-center text-gray-500 text-sm">© 2025 Medley. All rights reserved.</footer>
        </div>
      ) : !role ? (
        <div className="flex items-center justify-center min-h-screen">
          <div className="relative bg-white p-6 rounded-xl shadow-md border border-gray-100">
            <div className="absolute top-0 left-0 w-full h-2 rounded-t-xl bg-gradient-to-r from-blue-500 to-purple-500"></div>
            <h1 className="text-2xl font-bold mb-6 text-gray-800">Register</h1>
            <div className="space-y-4">
              <div>
                <label htmlFor="name" className="block text-sm font-medium text-gray-700 mb-1">Name</label>
                <input id="name" type="text" value={name} onChange={(e) => setName(e.target.value)} placeholder="Enter your name" className="w-full p-3 border border-gray-300 rounded-lg focus:ring-2 focus:ring-blue-500 focus:border-blue-500 transition-all duration-200" />
              </div>
              <div>
                <label htmlFor="role" className="block text-sm font-medium text-gray-700 mb-1">Role</label>
                <select id="role" value={selectedRole} onChange={(e) => setSelectedRole(e.target.value)} className="w-full p-3 border border-gray-300 rounded-lg focus:ring-2 focus:ring-blue-500 focus:border-blue-500 transition-all duration-200">
                  <option value="">Select a role</option>
                  <option value="Customer">Customer</option>
                  <option value="Organizer">Organizer</option>
                  <option value="Admin">Admin</option>
                </select>
              </div>
            </div>
            <button onClick={registerRole} className="mt-4 flex items-center px-3 py-1.5 rounded-lg text-sm font-medium bg-green-500 text-white hover:bg-green-600 transition-all duration-200">
              <svg className="w-4 h-4 mr-1" fill="none" stroke="currentColor" viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg"><path strokeLinecap="round" strokeLinejoin="round" strokeWidth="2" d="M5 13l4 4L19 7" /></svg>
              Register
            </button>
          </div>
        </div>
      ) : role === "Admin" ? (
        <AdminDashboard actor={actor} logout={logout} principal={principal} />
      ) : role === "Organizer" ? (
        <OrganizerDashboard actor={actor} logout={logout} principal={principal} refetchConcerts={fetchConcerts} />
      ) : (
        <CustomerDashboard actor={actor} logout={logout} principal={principal} fetchTickets={fetchTickets} fetchCustomerBalance={fetchCustomerBalance} tickets={tickets} concerts={ticketConcerts} balance={balance}/>
      )}
      {isAuthenticated && role && (
        <div className="p-6">
          <h2 className="text-2xl font-bold mb-6 text-gray-800">All Concerts</h2>
          {!tokenInitialized ? (
            <div><p className="text-gray-600">The token has not been initialized yet. Please wait for an Organizer to initialize the token.</p></div>
          ) : (
            <>
              <div className="mb-8 bg-white p-5 rounded-xl shadow-md">
                <form onSubmit={(e) => { e.preventDefault(); fetchConcerts(); }} className="flex flex-col md:flex-row gap-4 items-end">
                  <div className="flex-1">
                    <label htmlFor="search" className="block text-sm font-medium text-gray-700 mb-1">Search by Name</label>
                    <div className="relative">
                      <input id="search" type="text" value={search} onChange={(e) => setSearch(e.target.value)} placeholder="Enter concert name" className="w-full p-3 pl-10 border border-gray-300 rounded-lg focus:ring-2 focus:ring-blue-500 focus:border-blue-500 transition-all duration-200" />
                      <svg className="absolute left-3 top-1/2 transform -translate-y-1/2 w-5 h-5 text-gray-400" fill="none" stroke="currentColor" viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg"><path strokeLinecap="round" strokeLinejoin="round" strokeWidth="2" d="M21 21l-6-6m2-5a7 7 0 11-14 0 7 7 0 0114 0z" /></svg>
                    </div>
                  </div>
                  <div className="flex-1">
                    <label htmlFor="minPrice" className="block text-sm font-medium text-gray-700 mb-1">Min Price</label>
                    <div className="relative">
                      <input id="minPrice" type="number" value={minPrice} onChange={(e) => setMinPrice(e.target.value)} placeholder="Min Price" className="w-full p-3 pl-10 border border-gray-300 rounded-lg focus:ring-2 focus:ring-blue-500 focus:border-blue-500 transition-all duration-200" />
                      <svg className="absolute left-3 top-1/2 transform -translate-y-1/2 w-5 h-5 text-gray-400" fill="none" stroke="currentColor" viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg"><path strokeLinecap="round" strokeLinejoin="round" strokeWidth="2" d="M12 8c-1.657 0-3 .895-3 2s1.343 2 3 2 3 .895 3 2-1.343 2-3 2m0-8c1.11 0 2广东.402 2.599 1M12 8V7m0 1v8m0 0v1m0-1c-1.11 0-2.08-.402-2.599-1M21 12a9 9 0 11-18 0 9 9 0 0118 0z" /></svg>
                    </div>
                  </div>
                  <div className="flex-1">
                    <label htmlFor="maxPrice" className="block text-sm font-medium text-gray-700 mb-1">Max Price</label>
                    <div className="relative">
                      <input id="maxPrice" type="number" value={maxPrice} onChange={(e) => setMaxPrice(e.target.value)} placeholder="Max Price" className="w-full p-3 pl-10 border border-gray-300 rounded-lg focus:ring-2 focus:ring-blue-500 focus:border-blue-500 transition-all duration-200" />
                      <svg className="absolute left-3 top-1/2 transform -translate-y-1/2 w-5 h-5 text-gray-400" fill="none" stroke="currentColor" viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg"><path strokeLinecap="round" strokeLinejoin="round" strokeWidth="2" d="M12 8c-1.657 0-3 .895-3 2s1.343 2 3 2 3 .895 3 2-1.343 2-3 2m0-8c1.11 0 2.08.402 2.599 1M12 8V7m0 1v8m0 0v1m0-1c-1.11 0-2.08-.402-2.599-1M21 12a9 9 0 11-18 0 9 9 0 0118 0z" /></svg>
                    </div>
                  </div>
                  <div className="flex items-center">
                    <label htmlFor="onlyAvailable" className="flex items-center text-sm font-medium text-gray-700">
                      <input id="onlyAvailable" type="checkbox" checked={onlyAvailable} onChange={(e) => setOnlyAvailable(e.target.checked)} className="mr-2 h-4 w-4 text-blue-500 border-gray-300 rounded focus:ring-blue-500" />
                      Only Available
                    </label>
                  </div>
                  <div className="flex space-x-2">
                    <button type="submit" className="flex items-center px-4 py-2 rounded-lg text-sm font-medium bg-blue-500 text-white hover:bg-blue-600 transition-all duration-200">
                      <svg className="w-4 h-4 mr-2" fill="none" stroke="currentColor" viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg"><path strokeLinecap="round" strokeLinejoin="round" strokeWidth="2" d="M3 4a1 1 0 011-1h16a1 1 0 011 1v2.586a1 1 0 01-.293.707l-6.414 6.414a1 1 0 00-.293.707V17l-4 4v-6.586a1 1 0 00-.293-.707L3.293 7.293A1 1 0 013 6.586V4z" /></svg>
                      Apply Filters
                    </button>
                    <button type="button" onClick={() => { setSearch(""); setMinPrice(""); setMaxPrice(""); setOnlyAvailable(false); fetchConcerts(); }} className="flex items-center px-4 py-2 rounded-lg text-sm font-medium bg-gray-200 text-gray-700 hover:bg-gray-300 transition-all duration-200">
                      <svg className="w-4 h-4 mr-2" fill="none" stroke="currentColor" viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg"><path strokeLinecap="round" strokeLinejoin="round" strokeWidth="2" d="M6 18L18 6M6 6l12 12" /></svg>
                      Clear Filters
                    </button>
                  </div>
                </form>
              </div>
              {isLoading ? (
                <div className="text-center py-10">
                  <svg className="animate-spin h-8 w-8 text-blue-500 mx-auto" fill="none" viewBox="0 0 24 24">
                    <circle className="opacity-25" cx="12" cy="12" r="10" stroke="currentColor" strokeWidth="4" />
                    <path className="opacity-75" fill="currentColor" d="M4 12a8 8 0 018-8V0C5.373 0 0 5.373 0 12h4zm2 5.291A7.962 7.962 0 014 12H0c0 3.042 1.135 5.824 3 7.938l3-2.647z" />
                  </svg>
                  <p className="text-gray-500 mt-2">Loading concerts...</p>
                </div>
              ) : concerts.length === 0 ? (
                <div className="text-center py-10">
                  <p className="text-gray-500 text-lg">No concerts found.</p>
                  <p className="text-gray-400">Try adjusting your filters or check back later!</p>
                </div>
              ) : (
                <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-3 gap-6">
                  {concerts.map((concert) => (
                    <ConcertCard key={concert.id} concert={concert} onClick={setSelectedConcert} role={role} onBuyTicket={handleBuyTicket} />
                  ))}
                </div>
              )}
            </>
          )}
        </div>
      )}
      {selectedConcert && (
        <ConcertDetailsModal concert={selectedConcert} onClose={() => setSelectedConcert(null)} role={role} onBuyTicket={handleBuyTicket} />
      )}
    </div>
  );
}

export default App;