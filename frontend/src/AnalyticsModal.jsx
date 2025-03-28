import React from "react";

export default function AnalyticsModal({ concert, onClose }) {
  if (!concert) return null;
  const soldTickets = Number(concert.soldTickets);
  const totalTickets = Number(concert.totalTickets);
  const price = Number(concert.price);
  const revenue = soldTickets * price;
  
  return (
    <div className="fixed inset-0 bg-black bg-opacity-50 flex items-center justify-center z-50">
      <div className="relative bg-white p-6 rounded-xl shadow-lg max-w-md w-full border border-gray-100">
        <div className="absolute top-0 left-0 w-full h-2 rounded-t-xl bg-gradient-to-r from-blue-500 to-purple-500"></div>
        <h2 className="text-2xl font-bold mb-4 text-gray-800">{concert.name} Analytics</h2>
        <div className="space-y-3">
          <div className="flex justify-between"><span className="font-medium text-gray-700 text-sm">Tickets Sold:</span><span className="text-gray-600 text-sm">{soldTickets}/{totalTickets}</span></div>
          <div className="flex justify-between"><span className="font-medium text-gray-700 text-sm">Revenue:</span><span className="text-gray-600 text-sm">{revenue}</span></div>
          <div className="flex justify-between"><span className="font-medium text-gray-700 text-sm">Remaining Tickets:</span><span className="text-gray-600 text-sm">{totalTickets - soldTickets}</span></div>
        </div>
        <div className="mt-6 flex justify-end">
          <button onClick={onClose} className="flex items-center px-4 py-2 rounded-lg text-sm font-medium bg-gray-200 text-gray-700 hover:bg-gray-300 transition-all duration-200">
            <svg className="w-4 h-4 mr-2" fill="none" stroke="currentColor" viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg"><path strokeLinecap="round" strokeLinejoin="round" strokeWidth="2" d="M6 18L18 6M6 6l12 12" /></svg>
            Close
          </button>
        </div>
      </div>
    </div>
  );
}