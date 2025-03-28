import React from "react";

export default function ConcertDetailsModal({ concert, onClose, role, onBuyTicket }) {
  if (!concert) return null;
  const soldTickets = Number(concert.soldTickets);
  const totalTickets = Number(concert.totalTickets);
  const price = Number(concert.price);
  const currentTime = new Date();
  const concertDate = new Date(Number(concert.date) / 1000000);
  const isPastOrToday = concertDate <= currentTime;
  const status = isPastOrToday
    ? { text: "Not Available", color: "bg-gray-100 text-gray-800" }
    : soldTickets >= totalTickets
      ? { text: "Sold Out", color: "bg-red-100 text-red-800" }
      : soldTickets >= totalTickets * 0.8
        ? { text: "Few Tickets Left", color: "bg-Ayellow-100 text-yellow-800" }
        : { text: "Available", color: "bg-green-100 text-green-800" };
        
  return (
    <div className="fixed inset-0 bg-black bg-opacity-50 flex items-center justify-center">
      <div className="bg-white p-6 rounded-xl shadow-lg max-w-md w-full border border-gray-100">
        <div className="absolute top-0 left-0 w-full h-2 rounded-t-xl bg-gradient-to-r from-blue-500 to-purple-500"></div>
        <h2 className="text-2xl font-bold mb-4 text-gray-800">{concert.name}</h2>
        <div className="space-y-2">
          <p className="text-gray-600 text-sm"><span className="font-medium">Date:</span> {new Date(Number(concert.date) / 1000000).toLocaleDateString()}</p>
          <p className="text-gray-600 text-sm"><span className="font-medium">Total Tickets:</span> {totalTickets}</p>
          <p className="text-gray-600 text-sm"><span className="font-medium">Tickets Sold:</span> {soldTickets}</p>
          <p className="text-gray-600 text-sm"><span className="font-medium">Price:</span> {price}</p>
          <p className="text-gray-600 text-sm"><span className="font-medium">Organizer ID:</span> {concert.organizerId.toText()}</p>
        </div>
        <div className="mt-4 flex space-x-2">
          {role === "Customer" && soldTickets < totalTickets && !isPastOrToday && (
            <button onClick={() => onBuyTicket(concert.id)} className="flex items-center px-3 py-1.5 rounded-lg text-sm font-medium bg-blue-500 text-white hover:bg-blue-600 transition-all duration-200">
              <svg className="w-4 h-4 mr-1" fill="none" stroke="currentColor" viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg"><path strokeLinecap="round" strokeLinejoin="round" strokeWidth="2" d="M3 3h2l.4 2M7 13h10l4-8H5.4M7 13L5.4 5M7 13l-2.293 2.293c-.63.63-.184 1.707.707 1.707H17m0 0a2 2 0 100 4 2 2 0 000-4zm-8 2a2 2 0 11-4 0 2 2 0 014 0z" /></svg>
              Buy Ticket
            </button>
          )}
          <button onClick={onClose} className="flex items-center px-3 py-1.5 rounded-lg text-sm font-medium bg-gray-200 text-gray-700 hover:bg-gray-300 transition-all duration-200">
            <svg className="w-4 h-4 mr-1" fill="none" stroke="currentColor" viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg"><path strokeLinecap="round" strokeLinejoin="round" strokeWidth="2" d="M6 18L18 6M6 6l12 12" /></svg>
            Close
          </button>
        </div>
      </div>
    </div>
  );
}