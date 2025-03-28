import React from "react";

export default function ConcertCard({ concert, onClick, role, onBuyTicket }) {
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
    <div className="bg-white p-5 rounded-xl shadow-lg border border-gray-100 hover:shadow-xl transition-all duration-300 cursor-pointer" onClick={() => onClick(concert)}>
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
      <div className="mt-4 flex space-x-2">
        <button onClick={() => onClick(concert)} className="flex items-center px-3 py-1.5 rounded-lg text-sm font-medium bg-gray-100 text-gray-700 hover:bg-gray-200 transition-all duration-200" title="View Concert Details">
          <svg className="w-4 h-4 mr-1" fill="none" stroke="currentColor" viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg"><path strokeLinecap="round" strokeLinejoin="round" strokeWidth="2" d="M15 12a3 3 0 11-6 0 3 3 0 016 0z" /><path strokeLinecap="round" strokeLinejoin="round" strokeWidth="2" d="M2.458 12C3.732 7.943 7.523 5 12 5c4.478 0 8.268 2.943 9.542 7-1.274 4.057-5.064 7-9.542 7-4.477 0-8.268-2.943-9.542-7z" /></svg>
          View Details
        </button>
        {role === "Customer" && status.text !== "Sold Out" && !isPastOrToday && (
          <button onClick={(e) => { e.stopPropagation(); onBuyTicket(concert.id); }} className="flex items-center px-3 py-1.5 rounded-lg text-sm font-medium bg-blue-500 text-white hover:bg-blue-600 transition-all duration-200" title="Buy a Ticket">
            <svg className="w-4 h-4 mr-1" fill="none" stroke="currentColor" viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg"><path strokeLinecap="round" strokeLinejoin="round" strokeWidth="2" d="M3 3h2l.4 2M7 13h10l4-8H5.4M7 13L5.4 5M7 13l-2.293 2.293c-.63.63-.184 1.707.707 1.707H17m0 0a2 2 0 100 4 2 2 0 000-4zm-8 2a2 2 0 11-4 0 2 2 0 014 0z" /></svg>
            Buy Ticket
          </button>
        )}
      </div>
    </div>
  );
}