import React, { useState, useEffect } from "react";
import EditConcertForm from "./EditConcertForm";
import AnalyticsModal from "./AnalyticsModal";
import TokenInitializationForm from "./TokenInitializationForm";
import CreateConcertForm from "./CreateConcertForm";

export default function OrganizerDashboard({ actor, logout, principal, refetchConcerts }) {
  const [concerts, setConcerts] = useState([]);
  const [editingConcert, setEditingConcert] = useState(null);
  const [showAnalytics, setShowAnalytics] = useState(null);
  const [balance, setBalance] = useState(0);
  const [revenue, setRevenue] = useState(0);
  const [tokenInitialized, setTokenInitialized] = useState(false);

  useEffect(() => {
    checkTokenInitialization();
    fetchConcerts();
    fetchOrganizerData();
  }, [principal]);

  const checkTokenInitialization = async () => {
    try {
      const initialized = await actor.isTokenInitialized();
      setTokenInitialized(initialized);
    } catch (error) {
      console.error("Failed to check token initialization:", error);
      setTokenInitialized(false);
    }
  };

  const fetchConcerts = async () => {
    try {
      if (!principal) {
        console.error("Principal is not defined");
        return;
      }
      const concertIds = await actor.getOrganizerConcerts(principal);
      const concertPromises = concertIds.map(async (id) => {
        const [concert, found] = await actor.getConcert(id);
        return found ? concert : null;
      });
      const concertResults = await Promise.all(concertPromises);
      setConcerts(concertResults.filter((c) => c !== null));
    } catch (error) {
      console.error("Failed to fetch concerts:", error);
      setConcerts([]);
    }
    refetchConcerts();
  };

  const fetchOrganizerData = async () => {
    try {
      if (!principal) {
        console.error("Principal is not defined");
        return;
      }
      const bal = await actor.getOrganizerBalance(principal);
      const rev = await actor.getOrganizerRevenue(principal);
      setBalance(Number(bal) / 100_000_000);
      setRevenue(Number(rev) / 100_000_000);
    } catch (error) {
      console.error("Failed to fetch organizer data:", error);
      setBalance(0);
      setRevenue(0);
    }
  };

  const handleCreateConcert = async (name, date, totalTickets, price) => {
    try {
      await actor.createConcert(name, date, totalTickets, price);
      fetchConcerts();
      fetchOrganizerData();
    } catch (error) {
      alert(`Failed to create concert: ${error.message}`);
    }
  };

  const handleInitializeToken = async (tokenData) => {
    try {
      const result = await actor.initializeToken(tokenData);
      if (result.Ok) {
        setTokenInitialized(true);
        return result.Ok;
      } else {
        throw new Error(result.Err);
      }
    } catch (error) {
      throw error;
    }
  };

  const handleEditConcert = async (concertId, name, date, totalTickets, price) => {
    try {
      const success = await actor.editConcert(concertId, name, date, totalTickets, price);
      if (success) {
        fetchConcerts();
        return true;
      } else {
        alert("Failed to edit concert: Tickets may have been sold or you are not the organizer.");
        return false;
      }
    } catch (error) {
      alert(`Failed to edit concert: ${error.message}`);
      return false;
    }
  };

  const handleDeleteConcert = async (concertId) => {
    if (window.confirm("Are you sure you want to delete this concert?")) {
      try {
        const success = await actor.deleteConcert(concertId);
        if (success) {
          fetchConcerts();
        } else {
          alert("Failed to delete concert: Tickets may have been sold or you are not the organizer.");
        }
      } catch (error) {
        alert(`Failed to delete concert: ${error.message}`);
      }
    }
  };

  const handleValidateTicket = async (tokenId) => {
    try {
      const result = await actor.validateTicket(tokenId);
      alert(result);
      fetchConcerts();
    } catch (error) {
      alert(`Failed to validate ticket: ${error.message}`);
    }
  };
  
  return (
    <div className="p-6">
      <div className="flex justify-between items-center mb-6">
        <h1 className="text-2xl font-bold text-gray-800">Organizer Dashboard</h1>
        <button onClick={logout} className="flex items-center px-3 py-1.5 rounded-lg text-sm font-medium bg-red-500 text-white hover:bg-red-600 transition-all duration-200">
          <svg className="w-4 h-4 mr-1" fill="none" stroke="currentColor" viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg"><path strokeLinecap="round" strokeLinejoin="round" strokeWidth="2" d="M17 16l4-4m0 0l-4-4m4 4H7m6 4v1a3 3 0 01-3 3H6a3 3 0 01-3-3V7a3 3 0 013-3h4a3 3 0 013 3v1" /></svg>
          Logout
        </button>
      </div>
      <div className="mb-6 bg-white p-4 rounded-xl shadow-md">
        <h2 className="text-xl font-semibold text-gray-800 mb-2">Account Overview</h2>
        <p className="text-gray-600">Balance: {balance}</p>
        <p className="text-gray-600">Revenue: {revenue}</p>
      </div>
      {!tokenInitialized ? (
        <div>
          <h2 className="text-2xl font-bold mb-6 text-gray-800">Token Not Initialized</h2>
          <p className="text-gray-600 mb-4">You need to initialize the token before proceeding. Please provide the token details below.</p>
          <TokenInitializationForm onInitialize={handleInitializeToken} />
        </div>
      ) : (
        <>
          <CreateConcertForm onCreate={handleCreateConcert} />
          <h2 className="text-2xl font-bold mb-6 text-gray-800">My Concerts</h2>
          {concerts.length === 0 ? (
            <div className="text-center py-10">
              <p className="text-gray-500 text-lg">No concerts created yet.</p>
              <p className="text-gray-400">Use the form above to create a new concert!</p>
            </div>
          ) : (
            <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-3 gap-6">
              {concerts.map((concert) => {
                const soldTickets = Number(concert.soldTickets);
                const totalTickets = Number(concert.totalTickets);
                const currentTime = new Date();
                const concertDate = new Date(Number(concert.date) / 1000000);
                const isPastOrToday = concertDate <= currentTime;
                const status = isPastOrToday
                  ? { text: "Not Available", color: "bg-gray-100 text-gray-800" }
                  : soldTickets >= totalTickets
                    ? { text: "Sold Out", color: "bg-red-100 text-red-800" }
                    : soldTickets >= totalTickets * 0.8
                      ? { text: "Few Tickets Left", color: "bg-yellow-100 text-yellow-800" }
                      : { text: "Available", color: "bg-green-100 text-green-800" };

                return (
                  <div key={concert.id} className="relative bg-white p-5 rounded-xl shadow-lg border border-gray-100 hover:shadow-xl transition-all duration-300">
                    <div className="absolute top-0 left-0 w-full h-2 rounded-t-xl bg-gradient-to-r from-blue-500 to-purple-500"></div>
                    <div className="mt-2">
                      <h3 className="text-xl font-semibold text-gray-800 truncate">{concert.name}</h3>
                      <div className="mt-2 space-y-1">
                        <p className="text-gray-600 text-sm"><span className="font-medium">Date:</span> {new Date(Number(concert.date) / 1000000).toLocaleDateString()}</p>
                        <p className="text-gray-600 text-sm"><span className="font-medium">Tickets:</span> {soldTickets}/{totalTickets}</p>
                        <p className="text-gray-600 text-sm"><span className="font-medium">Price:</span> {Number(concert.price)}</p>
                      </div>
                      <span className={`inline-block mt-3 px-3 py-1 rounded-full text-xs font-medium ${status.color}`}>{status.text}</span>
                    </div>
                    <div className="mt-4 flex space-x-2 flex-wrap">
                      <button onClick={() => setEditingConcert(concert)} className={`flex items-center px-3 py-1.5 rounded-lg text-sm font-medium transition-all duration-200 ${Number(concert.soldTickets) > 0 ? "bg-gray-300 text-gray-500 cursor-not-allowed" : "bg-blue-500 text-white hover:bg-blue-600"}`} disabled={Number(concert.soldTickets) > 0} title={Number(concert.soldTickets) > 0 ? "Cannot edit: Tickets have been sold" : "Edit Concert"}>
                        <svg className="w-4 h-4 mr-1" fill="none" stroke="currentColor" viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg"><path strokeLinecap="round" strokeLinejoin="round" strokeWidth="2" d="M11 5H6a2 2 0 00-2 2v11a2 2 0 002 2h11a2 2 0 002-2v-5m-1.414-9.414a2 2 0 112.828 2.828L11.828 15.828a2 2 0 01-2.828 0l-1.414-1.414a2 2 0 010-2.828l9.414-9.414z" /></svg>
                        Edit
                      </button>
                      <button onClick={() => handleDeleteConcert(concert.id)} className={`flex items-center px-3 py-1.5 rounded-lg text-sm font-medium transition-all duration-200 ${Number(concert.soldTickets) > 0 ? "bg-gray-300 text-gray-500 cursor-not-allowed" : "bg-red-500 text-white hover:bg-red-600"}`} disabled={Number(concert.soldTickets) > 0} title={Number(concert.soldTickets) > 0 ? "Cannot delete: Tickets have been sold" : "Delete Concert"}>
                        <svg className="w-4 h-4 mr-1" fill="none" stroke="currentColor" viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg"><path strokeLinecap="round" strokeLinejoin="round" strokeWidth="2" d="M19 7l-.867 12.142A2 2 0 0116.138 21H7.862a2 2 0 01-1.995-1.858L5 7m5-4h4a1 1 0 011 1v1H9V4a1 1 0 011-1z" /></svg>
                        Delete
                      </button>
                      <button onClick={() => setShowAnalytics(concert)} className="flex items-center px-3 py-1.5 rounded-lg text-sm font-medium bg-purple-500 text-white hover:bg-purple-600 transition-all duration-200" title="View Analytics">
                        <svg className="w-4 h-4 mr-1" fill="none" stroke="currentColor" viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg"><path strokeLinecap="round" strokeLinejoin="round" strokeWidth="2" d="M9 19v-6a2 2 0 00-2-2H5a2 2 0 00-2 2v6a2 2 0 002 2h2a2 2 0 002-2zm0 0V9a2 2 0 012-2h2a2 2 0 012 2v10m-6 0a2 2 0 002 2h2a2 2 0 002-2V5a2 2 0 012-2h2a2 2 0 012 2v14a2 2 0 01-2 2h-2a2 2 0 01-2-2z" /></svg>
                        Analytics
                      </button>
                    </div>
                    <div className="mt-4">
                      <input type="text" placeholder="Enter ticket ID to validate" className="w-full p-2 border border-gray-300 rounded-lg" onKeyPress={(e) => { if (e.key === "Enter") { handleValidateTicket(e.target.value); e.target.value = ""; } }} />
                    </div>
                  </div>
                );
              })}
            </div>
          )}
        </>
      )}
      {editingConcert && <EditConcertForm concert={editingConcert} onEdit={handleEditConcert} onClose={() => setEditingConcert(null)} />}
      {showAnalytics && <AnalyticsModal concert={showAnalytics} onClose={() => setShowAnalytics(null)} />}
    </div>
  );
}