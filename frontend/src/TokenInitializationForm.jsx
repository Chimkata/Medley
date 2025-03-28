import React, { useState } from "react";

export default function TokenInitializationForm({ onInitialize }) {
  const [tokenName, setTokenName] = useState("");
  const [tokenSymbol, setTokenSymbol] = useState("");
  const [initialSupply, setInitialSupply] = useState("");
  const [tokenLogo, setTokenLogo] = useState("");
  
  const handleSubmit = async (e) => {
    e.preventDefault();
    try {
      const result = await onInitialize({
        token_name: tokenName,
        token_symbol: tokenSymbol,
        initial_supply: parseInt(initialSupply) * 100_000_000,
        token_logo: tokenLogo,
      });
      alert(result);
    } catch (error) {
      alert(`Failed to initialize token: ${error.message}`);
    }
  };

  return (
    <form onSubmit={handleSubmit} className="relative bg-white p-6 rounded-xl shadow-md mb-6 border border-gray-100">
      <div className="absolute top-0 left-0 w-full h-2 rounded-t-xl bg-gradient-to-r from-blue-500 to-purple-500"></div>
      <h2 className="text-2xl font-bold mb-6 text-gray-800">Initialize Token</h2>
      <div className="space-y-4">
        <div>
          <label htmlFor="tokenName" className="block text-sm font-medium text-gray-700 mb-1">Token Name</label>
          <input id="tokenName" type="text" value={tokenName} onChange={(e) => setTokenName(e.target.value)} placeholder="Enter token name" className="w-full p-3 border border-gray-300 rounded-lg focus:ring-2 focus:ring-blue-500 focus:border-blue-500 transition-all duration-200" required />
        </div>
        <div>
          <label htmlFor="tokenSymbol" className="block text-sm font-medium text-gray-700 mb-1">Token Symbol</label>
          <input id="tokenSymbol" type="text" value={tokenSymbol} onChange={(e) => setTokenSymbol(e.target.value)} placeholder="Enter token symbol" className="w-full p-3 border border-gray-300 rounded-lg focus:ring-2 focus:ring-blue-500 focus:border-blue-500 transition-all duration-200" required />
        </div>
        <div>
          <label htmlFor="initialSupply" className="block text-sm font-medium text-gray-700 mb-1">Initial Supply</label>
          <input id="initialSupply" type="number" value={initialSupply} onChange={(e) => setInitialSupply(e.target.value)} placeholder="Enter initial supply" className="w-full p-3 border border-gray-300 rounded-lg focus:ring-2 focus:ring-blue-500 focus:border-blue-500 transition-all duration-200" required />
        </div>
        <div>
          <label htmlFor="tokenLogo" className="block text-sm font-medium text-gray-700 mb-1">Token Logo URL</label>
          <input id="tokenLogo" type="text" value={tokenLogo} onChange={(e) => setTokenLogo(e.target.value)} placeholder="Enter token logo URL" className="w-full p-3 border border-gray-300 rounded-lg focus:ring-2 focus:ring-blue-500 focus:border-blue-500 transition-all duration-200" required />
        </div>
      </div>
      <button type="submit" className="mt-4 flex items-center px-3 py-1.5 rounded-lg text-sm font-medium bg-green-500 text-white hover:bg-green-600 transition-all duration-200">
        <svg className="w-4 h-4 mr-1" fill="none" stroke="currentColor" viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg"><path strokeLinecap="round" strokeLinejoin="round" strokeWidth="2" d="M12 4v16m8-8H4" /></svg>
        Initialize Token
      </button>
    </form>
  );
}