import React, { useState } from "react";

export default function CreateConcertForm({ onCreate }) {
  const [name, setName] = useState("");
  const [date, setDate] = useState("");
  const [totalTickets, setTotalTickets] = useState("");
  const [price, setPrice] = useState("");
  
  const handleSubmit = async (e) => {
    e.preventDefault();
    const dateInNanoseconds = BigInt(new Date(date).getTime()) * 1000000n;
    await onCreate(
      name,
      dateInNanoseconds,
      parseInt(totalTickets),
      parseInt(price),
    );
    setName("");
    setDate("");
    setTotalTickets("");
    setPrice("");
  };

  return (
    <form onSubmit={handleSubmit} className="relative bg-white p-6 rounded-xl shadow-md mb-6 border border-gray-100">
      <div className="absolute top-0 left-0 w-full h-2 rounded-t-xl bg-gradient-to-r from-blue-500 to-purple-500"></div>
      <h2 className="text-2xl font-bold mb-6 text-gray-800">Create Concert</h2>
      <div className="space-y-4">
        <div>
          <label htmlFor="concertName" className="block text-sm font-medium text-gray-700 mb-1">Concert Name</label>
          <input id="concertName" type="text" value={name} onChange={(e) => setName(e.target.value)} placeholder="Enter concert name" className="w-full p-3 border border-gray-300 rounded-lg focus:ring-2 focus:ring-blue-500 focus:border-blue-500 transition-all duration-200" required />
        </div>
        <div>
          <label htmlFor="concertDate" className="block text-sm font-medium text-gray-700 mb-1">Date</label>
          <input id="concertDate" type="date" value={date} onChange={(e) => setDate(e.target.value)} className="w-full p-3 border border-gray-300 rounded-lg focus:ring-2 focus:ring-blue-500 focus:border-blue-500 transition-all duration-200" required />
        </div>
        <div>
          <label htmlFor="totalTickets" className="block text-sm font-medium text-gray-700 mb-1">Total Tickets</label>
          <input id="totalTickets" type="number" value={totalTickets} onChange={(e) => setTotalTickets(e.target.value)} placeholder="Enter total tickets" className="w-full p-3 border border-gray-300 rounded-lg focus:ring-2 focus:ring-blue-500 focus:border-blue-500 transition-all duration-200" required />
        </div>
        <div>
          <label htmlFor="price" className="block text-sm font-medium text-gray-700 mb-1">Price per Ticket</label>
          <input id="price" type="number" value={price} onChange={(e) => setPrice(e.target.value)} placeholder="Enter price per ticket" className="w-full p-3 border border-gray-300 rounded-lg focus:ring-2 focus:ring-blue-500 focus:border-blue-500 transition-all duration-200" required />
        </div>
      </div>
      <button type="submit" className="mt-4 flex items-center px-3 py-1.5 rounded-lg text-sm font-medium bg-green-500 text-white hover:bg-green-600 transition-all duration-200">
        <svg className="w-4 h-4 mr-1" fill="none" stroke="currentColor" viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg"><path strokeLinecap="round" strokeLinejoin="round" strokeWidth="2" d="M12 4v16m8-8H4" /></svg>
        Create
      </button>
    </form>
  );
}