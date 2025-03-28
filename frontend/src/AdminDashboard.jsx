import React, { useState, useEffect } from "react";
import { Principal } from "@dfinity/principal";

export default function AdminDashboard({ actor, logout, principal }) {
    const [tokenInitialized, setTokenInitialized] = useState(false);
    const [tokenSettings, setTokenSettings] = useState(null);
    const [users, setUsers] = useState([]);
    const [transferTo, setTransferTo] = useState("");
    const [transferAmount, setTransferAmount] = useState("");
    const [balance, setBalance] = useState(0);
    const [tokenName, setTokenName] = useState("");
    const [tokenSymbol, setTokenSymbol] = useState("");
    const [initialSupply, setInitialSupply] = useState("");
    const [tokenLogo, setTokenLogo] = useState("");

    useEffect(() => {
        const checkRole = async () => {
            const role = await actor.getRole();
        };
        checkRole();
        checkTokenInitialization();
        fetchAdminBalance();
        if (tokenInitialized) {
            fetchTokenSettings();
            fetchAllUsers();
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

    const fetchAdminBalance = async () => {
        try {
            if (!principal || typeof principal !== "object" || !principal._isPrincipal) throw new Error("Invalid principal provided");
            const account = { owner: principal, subaccount: [] };
            const bal = await actor.icrc1_balance_of(account);
            setBalance(Number(bal) / 100_000_000);
        } catch (error) {
            console.error("Failed to fetch admin balance:", error);
            setBalance(0);
        }
    };

    const fetchTokenSettings = async () => {
        try {
            const settings = await actor.getTokenSettings();
            const tokenSettings = Array.isArray(settings) && settings.length > 0 ? settings[0] : null;
            if (tokenSettings) {
                setTokenSettings({
                    token_name: tokenSettings.token_name || "Not Set",
                    token_symbol: tokenSettings.token_symbol || "Not Set",
                    decimals: Number(tokenSettings.decimals) || 0,
                    transfer_fee: Number(tokenSettings.transfer_fee) / 100_000_000 || 0,
                    logo: tokenSettings.logo || "Not Set",
                    total_supply: Number(tokenSettings.total_supply) / 100_000_000 || 0,
                });
            } else {
                console.error("Token settings returned null or empty array. Caller might not be an admin or token not initialized.");
                setTokenSettings({ token_name: "Not Available", token_symbol: "Not Available", decimals: 0, transfer_fee: 0, logo: "Not Available", total_supply: 0 });
            }
        } catch (error) {
            console.error("Failed to fetch token settings:", error);
            setTokenSettings({ token_name: "Error", token_symbol: "Error", decimals: 0, transfer_fee: 0, logo: "Error", total_supply: 0 });
        }
    };

    const fetchAllUsers = async () => {
        try {
            setUsers([]);
            const userList = await actor.getAllUsers();
            if (userList) {
                const tuples = Array.isArray(userList[0]) && Array.isArray(userList[0][0]) ? userList[0] : userList;
                const mappedUsers = tuples.map((userTuple, index) => {
                    if (!Array.isArray(userTuple) || userTuple.length !== 3) {
                        console.error(`Invalid user tuple at index ${index}:`, userTuple);
                        return { role: "Unknown", name: "Unknown", balance: 0 };
                    }
                    const [role, name, balance] = userTuple;
                    return { role, name, balance: Number(balance) / 100_000_000 || 0 };
                });
                setUsers(mappedUsers);
            } else {
                console.error("User list returned null. Caller might not be an admin.");
                setUsers([]);
            }
        } catch (error) {
            console.error("Failed to fetch users:", error);
            setUsers([]);
        }
    };

    const handleInitializeToken = async (e) => {
        e.preventDefault();
        try {
            const result = await actor.initializeToken({
                token_name: tokenName,
                token_symbol: tokenSymbol,
                initial_supply: BigInt(Math.round(Number(initialSupply) * 100_000_000)),
                token_logo: tokenLogo,
            });
            if ("Ok" in result) {
                alert(result.Ok);
                setTokenInitialized(true);
                fetchTokenSettings();
                fetchAllUsers();
            } else {
                alert(result.Err);
            }
        } catch (error) {
            console.error("Failed to initialize token:", error);
            alert("Failed to initialize token.");
        }
    };

    const handleTransfer = async (e) => {
        e.preventDefault();
        try {
            const toPrincipal = Principal.fromText(transferTo);
            const amountNat = BigInt(Math.round(Number(transferAmount) * 100_000_000));
            const result = await actor.adminTransfer(toPrincipal, amountNat);
            if ("Ok" in result) {
                alert(`Transfer successful: Transaction ID ${result.Ok}`);
                setTransferTo("");
                setTransferAmount("");
                fetchAdminBalance();
                fetchAllUsers();
            } else {
                alert(
                  `Transfer failed: ${JSON.stringify(result.Err, (_, value) =>
                    typeof value === "bigint" ? value.toString() : value
                  )}`
                );
            }
        } catch (error) {
            console.error("Transfer failed:", error);
            alert("Failed to transfer tokens. Check the principal and amount.");
        }
    };
    
    return (
        <div className="p-6">
            <div className="flex justify-between items-center mb-6">
                <h1 className="text-2xl font-bold text-gray-800">Admin Dashboard</h1>
                <button onClick={logout} className="flex items-center px-3 py-1.5 rounded-lg text-sm font-medium bg-red-500 text-white hover:bg-red-600 transition-all duration-200">
                    <svg className="w-4 h-4 mr-1" fill="none" stroke="currentColor" viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg"><path strokeLinecap="round" strokeLinejoin="round" strokeWidth="2" d="M17 16l4-4m0 0l-4-4m4 4H7m6 4v1a3 3 0 01-3 3H6a3 3 0 01-3-3V7a3 3 0 013-3h4a3 3 0 013 3v1" /></svg>
                    Logout
                </button>
            </div>
            {!tokenInitialized ? (
                <div className="mb-6 bg-white p-4 rounded-xl shadow-md">
                    <h2 className="text-xl font-semibold text-gray-800 mb-4">Initialize Token</h2>
                    <form onSubmit={handleInitializeToken} className="space-y-4">
                        <div>
                            <label className="block text-sm font-medium text-gray-700 mb-1">Token Name</label>
                            <input type="text" value={tokenName} onChange={(e) => setTokenName(e.target.value)} placeholder="Enter token name" className="w-full p-3 border border-gray-300 rounded-lg focus:ring-2 focus:ring-blue-500 focus:border-blue-500 transition-all duration-200" required />
                        </div>
                        <div>
                            <label className="block text-sm font-medium text-gray-700 mb-1">Token Symbol</label>
                            <input type="text" value={tokenSymbol} onChange={(e) => setTokenSymbol(e.target.value)} placeholder="Enter token symbol" className="w-full p-3 border border-gray-300 rounded-lg focus:ring-2 focus:ring-blue-500 focus:border-blue-500 transition-all duration-200" required />
                        </div>
                        <div>
                            <label className="block text-sm font-medium text-gray-700 mb-1">Initial Supply</label>
                            <input type="number" value={initialSupply} onChange={(e) => setInitialSupply(e.target.value)} placeholder="Enter initial supply" className="w-full p-3 border border-gray-300 rounded-lg focus:ring-2 focus:ring-blue-500 focus:border-blue-500 transition-all duration-200" required />
                        </div>
                        <div>
                            <label className="block text-sm font-medium text-gray-700 mb-1">Token Logo URL</label>
                            <input type="text" value={tokenLogo} onChange={(e) => setTokenLogo(e.target.value)} placeholder="Enter logo URL" className="w-full p-3 border border-gray-300 rounded-lg focus:ring-2 focus:ring-blue-500 focus:border-blue-500 transition-all duration-200" required />
                        </div>
                        <button type="submit" className="flex items-center px-3 py-1.5 rounded-lg text-sm font-medium bg-blue-500 text-white hover:bg-blue-600 transition-all duration-200">
                            <svg className="w-4 h-4 mr-1" fill="none" stroke="currentColor" viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg"><path strokeLinecap="round" strokeLinejoin="round" strokeWidth="2" d="M5 13l4 4L19 7" /></svg>
                            Initialize Token
                        </button>
                    </form>
                </div>
            ) : (
                <>
                    <div className="mb-6 bg-white p-4 rounded-xl shadow-md">
                        <h2 className="text-xl font-semibold text-gray-800 mb-2">Account Overview</h2>
                        <p className="text-gray-600">Balance: {balance.toFixed(2)}</p>
                    </div>
                    <div className="mb-6 bg-white p-4 rounded-xl shadow-md">
                        <h2 className="text-xl font-semibold text-gray-800 mb-4">Token Settings</h2>
                        {tokenSettings ? (
                            <div className="space-y-2">
                                <p className="text-gray-600"><span className="font-medium">Name:</span> {tokenSettings.token_name}</p>
                                <p className="text-gray-600"><span className="font-medium">Symbol:</span> {tokenSettings.token_symbol}</p>
                                <p className="text-gray-600"><span className="font-medium">Decimals:</span> {tokenSettings.decimals}</p>
                                <p className="text-gray-600"><span className="font-medium">Transfer Fee:</span> {tokenSettings.transfer_fee.toFixed(8)}</p>
                                <p className="text-gray-600"><span className="font-medium">Total Supply:</span> {tokenSettings.total_supply.toFixed(2)}</p>
                                <p className="text-gray-600"><span className="font-medium">Logo:</span> <a href={tokenSettings.logo} target="_blank" className="text-blue-500 hover:underline">{tokenSettings.logo}</a></p>
                            </div>
                        ) : (
                            <p className="text-gray-500">Loading token settings...</p>
                        )}
                    </div>
                    <div className="mb-6 bg-white p-4 rounded-xl shadow-md">
                        <h2 className="text-xl font-semibold text-gray-800 mb-4">Transfer Tokens</h2>
                        <form onSubmit={handleTransfer} className="space-y-4">
                            <div>
                                <label className="block text-sm font-medium text-gray-700 mb-1">Recipient Principal</label>
                                <input type="text" value={transferTo} onChange={(e) => setTransferTo(e.target.value)} placeholder="Enter recipient principal" className="w-full p-3 border border-gray-300 rounded-lg focus:ring-2 focus:ring-blue-500 focus:border-blue-500 transition-all duration-200" required />
                            </div>
                            <div>
                                <label className="block text-sm font-medium text-gray-700 mb-1">Amount</label>
                                <input type="number" value={transferAmount} onChange={(e) => setTransferAmount(e.target.value)} placeholder="Enter amount" className="w-full p-3 border border-gray-300 rounded-lg focus:ring-2 focus:ring-blue-500 focus:border-blue-500 transition-all duration-200" required />
                            </div>
                            <button type="submit" className="flex items-center px-3 py-1.5 rounded-lg text-sm font-medium bg-green-500 text-white hover:bg-green-600 transition-all duration-200">
                                <svg className="w-4 h-4 mr-1" fill="none" stroke="currentColor" viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg"><path strokeLinecap="round" strokeLinejoin="round" strokeWidth="2" d="M5 13l4 4L19 7" /></svg>
                                Transfer
                            </button>
                        </form>
                    </div>
                    <div className="mb-6 bg-white p-4 rounded-xl shadow-md">
                        <h2 className="text-xl font-semibold text-gray-800 mb-4">All Users</h2>
                        {users.length === 0 ? (
                            <p className="text-gray-500">No users found.</p>
                        ) : (
                            <div className="overflow-x-auto">
                                <table className="min-w-full bg-white border border-gray-200">
                                    <thead>
                                        <tr className="bg-gray-100">
                                            <th className="py-2 px-4 border-b text-left text-sm font-medium text-gray-700">No.</th>
                                            <th className="py-2 px-4 border-b text-left text-sm font-medium text-gray-700">Name</th>
                                            <th className="py-2 px-4 border-b text-left text-sm font-medium text-gray-700">Role</th>
                                            <th className="py-2 px-4 border-b text-left text-sm font-medium text-gray-700">Balance</th>
                                        </tr>
                                    </thead>
                                    <tbody>
                                        {users.map((user, index) => (
                                            <tr key={index} className="hover:bg-gray-50">
                                                <td className="py-2 px-4 border-b text-sm text-gray-600">{index + 1}</td>
                                                <td className="py-2 px-4 border-b text-sm text-gray-600">{user.name}</td>
                                                <td className="py-2 px-4 border-b text-sm text-gray-600">{user.role}</td>
                                                <td className="py-2 px-4 border-b text-sm text-gray-600">{isNaN(user.balance) ? "Not Available" : user.balance.toFixed(2)}</td>
                                            </tr>
                                        ))}
                                    </tbody>
                                </table>
                            </div>
                        )}
                    </div>
                </>
            )}
        </div>
    );
}