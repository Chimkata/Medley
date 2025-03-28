import React, { useState } from "react";

export default function EditConcertForm({ concert, onEdit, onClose }) {
  const [name, setName] = useState(concert.name);
  const [date, setDate] = useState(new Date(Number(concert.date) / 1000000).toISOString().split("T")[0]);
  const [totalTickets, setTotalTickets] = useState(Number(concert.totalTickets).toString());
  const [price, setPrice] = useState(Number(concert.price).toString());
  
  const handleSubmit = async (e) => {
    e.preventDefault();
    const dateInNanoseconds = BigInt(new Date(date).getTime()) * 1000000n;
    const success = await onEdit(
      concert.id,
      name,
      dateInNanoseconds,
      parseInt(totalTickets),
      parseInt(price),
    );
    if (success) onClose();
  };

  return (
    <div className="fixed inset-0 bg-black bg-opacity-50 flex items-center justify-center z-[998]">
      <div className="bg-white p-6 rounded-xl shadow-lg max-w-md w-full border border-gray-100 z-[999]">
        <div className="absolute top-0 left-0 w-full h-2 rounded-t-xl bg-gradient-to-r from-blue-500 to-purple-500"></div>
        <h2 className="text-2xl font-bold mb-6 text-gray-800">Edit Concert</h2>
        <form onSubmit={handleSubmit}>
          <div className="space-y-4">
            <div>
              <label htmlFor="editConcertName" className="block text-sm font-medium text-gray-700 mb-1">Concert Name</label>
              <input id="editConcertName" type="text" value={name} onChange={(e) => setName(e.target.value)} placeholder="Enter concert name" className="w-full p-3 border border-gray-300 rounded-lg focus:ring-2 focus:ring-blue-500 focus:border-blue-500 transition-all duration-200" required />
            </div>
            <div>
              <label htmlFor="editConcertDate" className="block text-sm font-medium text-gray-700 mb-1">Date</label>
              <input id="editConcertDate" type="date" value={date} onChange={(e) => setDate(e.target.value)} className="w-full p-3 border border-gray-300 rounded-lg focus:ring-2 focus:ring-blue-500 focus:border-blue-500 transition-all duration-200" required />
            </div>
            <div>
              <label htmlFor="editTotalTickets" className="block text-sm font-medium text-gray-700 mb-1">Total Tickets</label>
              <input id="editTotalTickets" type="number" value={totalTickets} onChange={(e) => setTotalTickets(e.target.value)} placeholder="Enter total tickets" className="w-full p-3 border border-gray-300 rounded-lg focus:ring-2 focus:ring-blue-500 focus:border-blue-500 transition-all duration-200" required />
            </div>
            <div>
              <label htmlFor="editPrice" className="block text-sm font-medium text-gray-700 mb-1">Price per Ticket</label>
              <input id="editPrice" type="number" value={price} onChange={(e) => setPrice(e.target.value)} placeholder="Enter price per ticket" className="w-full p-3 border border-gray-300 rounded-lg focus:ring-2 focus:ring-blue-500 focus:border-blue-500 transition-all duration-200" required />
            </div>
          </div>
          <div className="mt-4 flex space-x-2">
            <button type="submit" className="flex items-center px-3 py-1.5 rounded-lg text-sm font-medium bg-blue-500 text-white hover:bg-blue-600 transition-all duration-200">
              <svg className="w-4 h-4 mr-1" fill="none" stroke="currentColor" viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg"><path strokeLinecap="round" strokeLinejoin="round" strokeWidth="2" d="M5 13l4 4L19 7" /></svg>
              Save
            </button>
            <button type="button" onClick={onClose} className="flex items-center px-3 py-1.5 rounded-lg text-sm font-medium bg-gray-200 text-gray-700 hover:bg-gray-300 transition-all duration-200">
              <svg className="w-4 h-4 mr-1" fill="none" stroke="currentColor" viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg"><path strokeLinecap="round" strokeLinejoin="round" strokeWidth="2" d="M6 18L18 6M6 6l12 12" /></svg>
              Cancel
            </button>
          </div>
        </form>
      </div>
    </div>
  );
}