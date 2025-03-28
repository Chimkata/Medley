import React, { useState, useEffect } from "react";

export default function CustomerDashboard({ actor, logout, principal, fetchTickets, fetchCustomerBalance, tickets, concerts, balance }) {
  const [selectedNFT, setSelectedNFT] = useState(null);
  const [tokenInitialized, setTokenInitialized] = useState(false);
  
  useEffect(() => {
    checkTokenInitialization();
    if (tokenInitialized) {
      fetchTickets();
      fetchCustomerBalance();
    }
  }, [principal, tokenInitialized]);

  const checkTokenInitialization = async () => {
    try {
      const initialized = await actor.isTokenInitialized();
      setTokenInitialized(initialized);
    } catch (error) {
      console.error("Failed to check token initialization:", error);
      setTokenInitialized(false);
    }
  };

  const viewNFT = async (tokenId) => {
    try {
      const metadata = await actor.icrc7_token_metadata(tokenId);
      if (!metadata) {
        console.error("NFT not found for tokenId:", tokenId);
        alert("NFT not found.");
        return;
      }
      const ownerResult = await actor.icrc7_owner_of(tokenId);
      const owner = ownerResult || null;
      setSelectedNFT({ tokenId, metadata: metadata || [], owner });
    } catch (error) {
      console.error("Failed to fetch NFT details:", error);
      alert("Failed to fetch NFT details.");
    }
  };

  return (
    <div className="p-6">
      <div className="flex justify-between items-center mb-6">
        <h1 className="text-2xl font-bold text-gray-800">Customer Dashboard</h1>
        <button onClick={logout} className="flex items-center px-3 py-1.5 rounded-lg text-sm font-medium bg-red-500 text-white hover:bg-red-600 transition-all duration-200">
          <svg className="w-4 h-4 mr-1" fill="none" stroke="currentColor" viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg"><path strokeLinecap="round" strokeLinejoin="round" strokeWidth="2" d="M17 16l4-4m0 0l-4-4m4 4H7m6 4v1a3 3 0 01-3 3H6a3 3 0 01-3-3V7a3 3 0 013-3h4a3 3 0 013 3v1" /></svg>
          Logout
        </button>
      </div>
      {!tokenInitialized ? (
        <div>
          <h2 className="text-2xl font-bold mb-6 text-gray-800">Token Not Initialized</h2>
          <p className="text-gray-600">The token has not been initialized yet. Please wait for an Organizer to initialize the token.</p>
        </div>
      ) : (
        <>
          <div className="mb-6 bg-white p-4 rounded-xl shadow-md">
            <h2 className="text-xl font-semibold text-gray-800 mb-2">Account Overview</h2>
            <p className="text-gray-600">Balance: {balance}</p>
          </div>
          <h2 className="text-2xl font-bold mb-6 text-gray-800">My NFT Tickets</h2>
          {tickets.length === 0 ? (
            <div className="text-center py-10">
              <p className="text-gray-500 text-lg">No tickets purchased yet.</p>
              <p className="text-gray-400">Check out the concerts below to buy tickets!</p>
            </div>
          ) : (
            <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-3 gap-6">
              {tickets.map((ticket, index) => {
                const concert = concerts[index];
                const currentTime = new Date();
                const concertDate = concert ? new Date(Number(concert.date) / 1000000) : null;
                const isPast = concertDate && concertDate < currentTime;
                const statusText = ticket.isValid
                  ? "Valid"
                  : isPast
                    ? "Expired (Concert Date Passed)"
                    : "Used";
                return (
                  <div key={ticket.id} className="relative bg-white p-5 rounded-xl shadow-lg border border-gray-100 hover:shadow-xl transition-all duration-300">
                    <div className="absolute top-0 left-0 w-full h-2 rounded-t-xl bg-gradient-to-r from-blue-500 to-purple-500"></div>
                    <h3 className="text-xl font-semibold text-gray-800 truncate">{concert ? concert.name : "Loading..."}</h3>
                    <div className="mt-2 space-y-1">
                      <p className="text-gray-600 text-sm"><span className="font-medium">NFT Token ID:</span> {ticket.id}</p>
                      <p className="text-gray-600 text-sm"><span className="font-medium">Date:</span> {concertDate ? concertDate.toLocaleDateString() : "N/A"}</p>
                      <p className="text-gray-600 text-sm"><span className="font-medium">Status:</span> <span className={`inline-block px-3 py-1 rounded-full text-xs font-medium ${ticket.isValid ? "bg-green-100 text-green-800" : "bg-red-100 text-red-800"}`}>{statusText}</span></p>
                    </div>
                    <button onClick={() => viewNFT(ticket.id)} className="mt-4 flex items-center px-3 py-1.5 rounded-lg text-sm font-medium bg-purple-500 text-white hover:bg-purple-600 transition-all duration-200">
                      <svg className="w-4 h-4 mr-1" fill="none" stroke="currentColor" viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg"><path strokeLinecap="round" strokeLinejoin="round" strokeWidth="2" d="M15 12a3 3 0 11-6 0 3 3 0 016 0z" /><path strokeLinecap="round" strokeLinejoin="round" strokeWidth="2" d="M2.458 12C3.732 7.943 7.523 5 12 5c4.478 0 8.268 2.943 9.542 7-1.274 4.057-5.064 7-9.542 7-4.477 0-8.268-2.943-9.542-7z" /></svg>
                      View NFT
                    </button>
                  </div>
                );
              })}
            </div>
          )}
        </>
      )}
      {selectedNFT && (
        <div className="fixed inset-0 bg-black bg-opacity-50 flex items-center justify-center z-50">
          <div className="relative bg-white p-6 rounded-xl shadow-lg max-w-md w-full border border-gray-100">
            <div className="absolute top-0 left-0 w-full h-2 rounded-t-xl bg-gradient-to-r from-blue-500 to-purple-500"></div>
            <h2 className="text-2xl font-bold mb-4 text-gray-800">NFT Ticket Details</h2>
            <div className="space-y-3">
              <div className="flex justify-between">
                <span className="font-medium text-gray-700 text-sm">Token ID:</span>
                <span className="text-gray-600 text-sm break-all">{selectedNFT.tokenId}</span>
              </div>
              <div className="flex justify-between">
                <span className="font-medium text-gray-700 text-sm">Owner:</span>
                <span className="text-gray-600 text-sm break-all">{selectedNFT.owner || "Unknown"}</span>
              </div>

              {selectedNFT.metadata.map((item) => {
                const [c_key, c_value] = item[0];
                const [d_key, d_value] = item[1];

                return (
                  <div>
                    <div className="flex justify-between">
                      <span className="font-medium text-gray-700 text-sm">Concert Name:</span>
                      <span className="text-gray-600 text-sm break-all">{c_value}</span>
                    </div>
                    
                    <div className="flex justify-between">
                      <span className="font-medium text-gray-700 text-sm">Date:</span>
                      <span className="text-gray-600 text-sm break-all">{new Date(Number(d_value)).toLocaleDateString()}</span>
                    </div>
                  </div>
                );
              })}

            </div>
            <div className="mt-6 flex justify-end">
              <button onClick={() => setSelectedNFT(null)} className="flex items-center px-4 py-2 rounded-lg text-sm font-medium bg-gray-200 text-gray-700 hover:bg-gray-300 transition-all duration-200">
                <svg className="w-4 h-4 mr-2" fill="none" stroke="currentColor" viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg"><path strokeLinecap="round" strokeLinejoin="round" strokeWidth="2" d="M6 18L18 6M6 6l12 12" /></svg>
                Close
              </button>
            </div>
          </div>
        </div>
      )}
    </div>
  );
}