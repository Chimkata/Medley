import Array "mo:base/Array";
import HashMap "mo:base/HashMap";
import Iter "mo:base/Iter";
import Nat "mo:base/Nat";
import Principal "mo:base/Principal";
import Text "mo:base/Text";
import Time "mo:base/Time";
import Option "mo:base/Option";
import Int "mo:base/Int";
import Blob "mo:base/Blob";
import Buffer "mo:base/Buffer";
import Nat8 "mo:base/Nat8";
import Nat64 "mo:base/Nat64";

actor Medley {
    stable var concertEntries : [(Text, Concert)] = [];
    stable var customerEntries : [(Principal, Customer)] = [];
    stable var organizerEntries : [(Principal, Organizer)] = [];
    stable var adminEntries : [(Principal, Admin)] = [];
    stable var nftEntries : [(Text, NFT)] = [];
    stable var platformRevenue: Nat = 0;

    stable var init : {
        initial_mints : [{
            account : { owner : Principal; subaccount : ?Blob };
            amount : Nat;
        }];
        minting_account : { owner : Principal; subaccount : ?Blob };
        token_name : Text;
        token_symbol : Text;
        decimals : Nat8;
        transfer_fee : Nat;
    } = {
        initial_mints = [];
        minting_account = { owner = Principal.fromBlob("\04"); subaccount = null };
        token_name = "";
        token_symbol = "";
        decimals = 0;
        transfer_fee = 0;
    };

    stable var logo : Text = "";
    stable var created : Bool = false;
    var log : Buffer.Buffer<Transaction> = Buffer.Buffer<Transaction>(0);
    stable var persistedLog : [Transaction] = [];
    var concerts : HashMap.HashMap<Text, Concert> = HashMap.fromIter(concertEntries.vals(), 10, Text.equal, Text.hash);
    var customers : HashMap.HashMap<Principal, Customer> = HashMap.fromIter(customerEntries.vals(), 10, Principal.equal, Principal.hash);
    var organizers : HashMap.HashMap<Principal, Organizer> = HashMap.fromIter(organizerEntries.vals(), 10, Principal.equal, Principal.hash);
    var admins : HashMap.HashMap<Principal, Admin> = HashMap.fromIter(adminEntries.vals(), 10, Principal.equal, Principal.hash); // New admin HashMap
    var nfts : HashMap.HashMap<Text, NFT> = HashMap.fromIter(nftEntries.vals(), 10, Text.equal, Text.hash);
    
    public type Timestamp = Nat64;
    public type Duration = Nat64;
    public type Subaccount = Blob;
    public type Account = { owner : Principal; subaccount : ?Subaccount };
    public type Tokens = Nat;
    public type Memo = Blob;
    public type TxIndex = Nat;
    public type TxLog = Buffer.Buffer<Transaction>;
    public type Value = { #Nat : Nat; #Int : Int; #Blob : Blob; #Text : Text };

    public type Operation = {
        #Approve : Approve;
        #Transfer : Transfer;
        #Burn : Transfer;
        #Mint : Transfer;
    };

    public type CommonFields = {
        memo : ?Memo;
        fee : ?Tokens;
        created_at_time : ?Timestamp;
    };

    public type Approve = CommonFields and {
        from : Account;
        spender : Account;
        amount : Nat;
        expires_at : ?Nat64;
    };

    public type TransferSource = {
        #Init;
        #Icrc1Transfer;
        #Icrc2TransferFrom;
    };

    public type Transfer = CommonFields and {
        spender : Account;
        source : TransferSource;
        to : Account;
        from : Account;
        amount : Tokens;
    };

    public type Transaction = {
        operation : Operation;
        fee : Tokens;
        timestamp : Timestamp;
    };

    public type TransferError = {
        #TooOld;
        #Duplicate : { duplicate_of : TxIndex };
        #CreatedInFuture : { ledger_time : Timestamp };
        #InsufficientFunds : { balance : Tokens };
        #BadFee : { expected_fee : Tokens };
        #TemporarilyUnavailable;
        #GenericError : { error_code : Nat; message : Text };
        #BadBurn : { min_burn_amount : Tokens };
    };

    public type Result<T, E> = { #Ok : T; #Err : E };
    private let platformFeePercentage: Nat = 10;
    private let platformAccount: Account = { owner = Principal.fromActor(Medley); subaccount = null };
    private let defaultSubaccount : Subaccount = Blob.fromArrayMut(Array.init(32, 0 : Nat8));
    private let maxMemoSize = 32;
    private let permittedDriftNanos : Duration = 60_000_000_000;
    private let transactionWindowNanos : Duration = 24 * 60 * 60 * 1_000_000_000;
    public type Concert = { id: Text; name: Text; date: Time.Time; totalTickets: Nat; soldTickets: Nat; price: Nat; organizerId: Principal; };
    public type Ticket = { id: Text; concertId: Text; owner: Principal; isValid: Bool; };
    public type Customer = { id: Principal; name: Text; tickets: [Text]; };
    public type Organizer = { id: Principal; name: Text; concerts: [Text]; revenue: Nat; };
    public type Admin = { id: Principal; name: Text; };

    public type NFT = {
        tokenId: Text;
        concertId: Text;
        owner: Principal;
        metadata: [(Text, Text)];
        used: Bool;
    };

    private let emptyConcert: Concert = { id = ""; name = ""; date = 0; totalTickets = 0; soldTickets = 0; price = 0; organizerId = Principal.fromText("aaaaa-aa"); };
    
    system func preupgrade() { 
        concertEntries := Iter.toArray(concerts.entries()); 
        customerEntries := Iter.toArray(customers.entries()); 
        organizerEntries := Iter.toArray(organizers.entries()); 
        adminEntries := Iter.toArray(admins.entries()); // Store admins
        nftEntries := Iter.toArray(nfts.entries()); 
        persistedLog := Buffer.toArray(log);
    };

    system func postupgrade() { 
        concerts := HashMap.fromIter(concertEntries.vals(), 10, Text.equal, Text.hash); 
        customers := HashMap.fromIter(customerEntries.vals(), 10, Principal.equal, Principal.hash); 
        organizers := HashMap.fromIter(organizerEntries.vals(), 10, Principal.equal, Principal.hash); 
        admins := HashMap.fromIter(adminEntries.vals(), 10, Principal.equal, Principal.hash);
        nfts := HashMap.fromIter(nftEntries.vals(), 10, Text.equal, Text.hash); 
        concertEntries := []; 
        customerEntries := []; 
        organizerEntries := []; 
        adminEntries := []; 
        nftEntries := []; 
        log := Buffer.Buffer<Transaction>(persistedLog.size());
        for (tx in Array.vals(persistedLog)) { log.add(tx); };
    };

    public shared ({ caller }) func initializeToken({
        token_name : Text;
        token_symbol : Text;
        initial_supply : Nat;
        token_logo : Text;
    }) : async Result<Text, Text> {
        if (created) { return #Err("Token already created"); };
        if (Principal.isAnonymous(caller)) { return #Err("Cannot create token with anonymous principal"); };
        if (Option.isNull(admins.get(caller))) {
            return #Err("Only an Admin can initialize the token");
        };
        init := {
            initial_mints = [{
                account = platformAccount;
                amount = initial_supply;
            }];
            minting_account = platformAccount;
            token_name;
            token_symbol;
            decimals = 8;
            transfer_fee = 10_000;
        };
        logo := token_logo;
        log := makeGenesisChain();
        created := true;
        #Ok("Medley token initialized with " # Nat.toText(initial_supply) # " tokens minted to platform")
    };

    public query func isTokenInitialized() : async Bool {
        created
    };

    private func accountsEqual(lhs : Account, rhs : Account) : Bool {
        let lhsSubaccount = Option.get(lhs.subaccount, defaultSubaccount);
        let rhsSubaccount = Option.get(rhs.subaccount, defaultSubaccount);
        Principal.equal(lhs.owner, rhs.owner) and Blob.equal(lhsSubaccount, rhsSubaccount);
    };

    private func balance(account : Account, log : TxLog) : Nat {
        var sum = 0;
        for (tx in log.vals()) {
            switch (tx.operation) {
                case (#Burn(args)) { if (accountsEqual(args.from, account)) { sum -= args.amount }; };
                case (#Mint(args)) { if (accountsEqual(args.to, account)) { sum += args.amount }; };
                case (#Transfer(args)) {
                    if (accountsEqual(args.from, account)) { sum -= args.amount + tx.fee; };
                    if (accountsEqual(args.to, account)) { sum += args.amount };
                };
                case (#Approve(args)) { if (accountsEqual(args.from, account)) { sum -= tx.fee }; };
            };
        };
        sum;
    };

    private func validateSubaccount(s : ?Subaccount) {
        let subaccount = Option.get(s, defaultSubaccount);
        assert (subaccount.size() == 32);
    };

    private func validateMemo(m : ?Memo) {
        switch (m) { 
            case (null) {}; 
            case (?memo) { 
                assert (memo.size() <= maxMemoSize) 
            }; 
        };
    };

    private func checkTxTime(created_at_time : ?Timestamp, now : Timestamp) : Result<(), TransferError> {
        let txTime : Timestamp = Option.get(created_at_time, now);
        if ((txTime > now) and (txTime - now > permittedDriftNanos)) {
            return #Err(#CreatedInFuture { ledger_time = now });
        };

        if ((txTime < now) and (now - txTime > transactionWindowNanos + permittedDriftNanos)) {
            return #Err(#TooOld);
        };

        #Ok(());
    };

    private func makeGenesisChain() : TxLog {
        validateSubaccount(init.minting_account.subaccount);
        let now = Nat64.fromNat(Int.abs(Time.now()));
        let log = Buffer.Buffer<Transaction>(100);

        for ({ account; amount } in Array.vals(init.initial_mints)) {
            validateSubaccount(account.subaccount);
            let tx : Transaction = {
                operation = #Mint({
                    spender = init.minting_account;
                    source = #Init;
                    from = init.minting_account;
                    to = account;
                    amount = amount;
                    fee = null;
                    memo = null;
                    created_at_time = ?now;
                });
                fee = 0;
                timestamp = now;
            };
            log.add(tx);
        };
        log;
    };

    private func classifyTransfer(log : TxLog, transfer : Transfer) : Result<(Operation, Tokens), TransferError> {
        let minter = init.minting_account;

        if (Option.isSome(transfer.created_at_time)) {
            switch (findTransfer(transfer, log)) {
                case (?txid) { return #Err(#Duplicate { duplicate_of = txid }) };
                case null {};
            };
        };

        let result = if (accountsEqual(transfer.from, minter) and transfer.source == #Init) {
            if (Option.get(transfer.fee, 0) != 0) { return #Err(#BadFee { expected_fee = 0 }); };
            (#Mint(transfer), 0);
        } else if (accountsEqual(transfer.to, minter) and transfer.source != #Icrc1Transfer) {
            if (Option.get(transfer.fee, 0) != 0) { return #Err(#BadFee { expected_fee = 0 }); };
            if (transfer.amount < init.transfer_fee) { return #Err(#BadBurn { min_burn_amount = init.transfer_fee }); };
            let debitBalance = balance(transfer.from, log);
            if (debitBalance < transfer.amount) { return #Err(#InsufficientFunds { balance = debitBalance }); };
            (#Burn(transfer), 0);
        } else {
            let effectiveFee = init.transfer_fee;
            if (Option.get(transfer.fee, effectiveFee) != effectiveFee) { return #Err(#BadFee { expected_fee = init.transfer_fee }); };
            let debitBalance = balance(transfer.from, log);
            if (debitBalance < transfer.amount + effectiveFee) { return #Err(#InsufficientFunds { balance = debitBalance }); };
            (#Transfer(transfer), effectiveFee);
        };

        #Ok(result);
    };

    private func findTransfer(transfer : Transfer, log : TxLog) : ?TxIndex {
        var i = 0;
        for (tx in log.vals()) {
            switch (tx.operation) {
                case (#Burn(args)) { if (args == transfer) { return ?i }; };
                case (#Mint(args)) { if (args == transfer) { return ?i }; };
                case (#Transfer(args)) { if (args == transfer) { return ?i }; };
                case (_) {};
            };
            i += 1;
        };
        null;
    };

    private func recordTransaction(tx : Transaction) : TxIndex {
        let idx = log.size();
        log.add(tx);
        idx;
    };

    private func applyTransfer(args : Transfer) : Result<TxIndex, TransferError> {
        validateSubaccount(args.from.subaccount);
        validateSubaccount(args.to.subaccount);
        validateMemo(args.memo);
        let now = Nat64.fromNat(Int.abs(Time.now()));
        switch (checkTxTime(args.created_at_time, now)) {
            case (#Ok(_)) {};
            case (#Err(e)) { 
                return #Err(e); 
            };
        };

        switch (classifyTransfer(log, args)) {
            case (#Ok((operation, effectiveFee))) {
                #Ok(recordTransaction({ 
                    operation; 
                    fee = effectiveFee; 
                    timestamp = now 
                }));
            };
            case (#Err(e)) { 
                #Err(e); 
            };
        };
    };

    public shared ({ caller }) func icrc1_transfer({
        from_subaccount : ?Subaccount;
        to : Account;
        amount : Tokens;
        fee : ?Tokens;
        memo : ?Memo;
        created_at_time : ?Timestamp;
    }) : async Result<TxIndex, TransferError> {
        let from = { owner = caller; subaccount = from_subaccount };
        applyTransfer({
            spender = from;
            source = #Icrc1Transfer;
            from = from;
            to = to;
            amount = amount;
            fee = fee;
            memo = memo;
            created_at_time = created_at_time;
        });
    };

    public query func icrc1_balance_of(account : Account) : async Tokens {
        balance(account, log);
    };

    public query func icrc1_name() : async Text { init.token_name };
    public query func icrc1_symbol() : async Text { init.token_symbol };
    public query func icrc1_decimals() : async Nat8 { init.decimals };
    public query func icrc1_fee() : async Nat { init.transfer_fee };

    public shared ({ caller }) func register(role: Text, name: Text) : async Text {
        if (Option.isSome(customers.get(caller)) or Option.isSome(organizers.get(caller)) or Option.isSome(admins.get(caller))) { 
            return "Error: Already registered"; 
        };
        if (not created and role != "Admin") {
            return "Error: Token not initialized. Only Admin can register before token initialization.";
        };
        if (role == "Admin" and admins.size() > 0) {
            return "Error: An Admin already exists. Only one Admin is allowed.";
        };
        let userName = name;
        let tokensToDistribute = 100 * (10 ** Nat8.toNat(init.decimals));
        let platformBalance = balance(platformAccount, log);
        let totalRequired = tokensToDistribute + init.transfer_fee;
        if (role == "Admin" and not created) {
            let newAdmin = { id = caller; name = userName };
            admins.put(caller, newAdmin);
            return "Registered as Admin: " # userName # " (Token not yet initialized)";
        };
        if (platformBalance < totalRequired) {
            return "Error: Platform lacks funds to distribute tokens";
        };
        let userAccount: Account = { owner = caller; subaccount = null };
        let transfer : Transfer = {
            spender = platformAccount;
            source = #Icrc1Transfer;
            from = platformAccount;
            to = userAccount;
            amount = tokensToDistribute;
            fee = ?init.transfer_fee;
            memo = null;
            created_at_time = null;
        };
        switch (applyTransfer(transfer)) {
            case (#Err(err)) { 
                return "Error: Failed to distribute tokens - " # debug_show(err); 
            };
            case (#Ok(_)) {
                switch (role) {
                    case "Customer" { 
                        let newCust = { id = caller; name = userName; tickets = [] };
                        customers.put(caller, newCust); 
                        return "Registered as Customer: " # userName # " with 100 tokens";
                    };
                    case "Organizer" { 
                        let newOrg = { id = caller; name = userName; concerts = []; revenue = 0 }; 
                        organizers.put(caller, newOrg); 
                        return "Registered as Organizer: " # userName # " with 100 tokens";
                    };
                    case "Admin" { 
                        let newAdmin = { id = caller; name = userName };
                        admins.put(caller, newAdmin); 
                        return "Registered as Admin: " # userName # " with 100 tokens";
                    };
                    case _ { 
                        return "Error: Invalid role"; 
                    };
                };
            };
        };
    };

    public query ({ caller }) func getRole() : async Text {
        if (Option.isSome(admins.get(caller))) { return "Admin"; }
        else if (Option.isSome(customers.get(caller))) { return "Customer"; } 
        else if (Option.isSome(organizers.get(caller))) { return "Organizer"; } 
        else { return ""; };
    };

    public query func getCustomerBalance(caller: Principal) : async Nat {
        let account: Account = { owner = caller; subaccount = null };
        balance(account, log)
    };

    public query func getOrganizerBalance(caller: Principal) : async Nat {
        let account: Account = { owner = caller; subaccount = null };
        balance(account, log)
    };

    public query ({ caller }) func getTokenSettings() : async ?{
        token_name : Text;
        token_symbol : Text;
        decimals : Nat8;
        transfer_fee : Nat;
        logo : Text;
        total_supply : Nat;
    } {
        if (Option.isNull(admins.get(caller))) { return null; };
        let totalSupply = balance(platformAccount, log);
        ?{
            token_name = init.token_name;
            token_symbol = init.token_symbol;
            decimals = init.decimals;
            transfer_fee = init.transfer_fee;
            logo = logo;
            total_supply = totalSupply;
        }
    };

    public shared ({ caller }) func adminTransfer(to: Principal, amount: Nat) : async Result<TxIndex, TransferError> {
        if (Option.isNull(admins.get(caller))) { return #Err(#GenericError { error_code = 1; message = "Only Admin can transfer tokens" }); };
        let fromAccount: Account = { owner = caller; subaccount = null };
        let toAccount: Account = { owner = to; subaccount = null };
        applyTransfer({
            spender = fromAccount;
            source = #Icrc1Transfer;
            from = fromAccount;
            to = toAccount;
            amount = amount;
            fee = ?init.transfer_fee;
            memo = null;
            created_at_time = null;
        });
    };

    public shared ({ caller }) func getAllUsers() : async ?[(Text, Text, Nat)] {
        if (Option.isNull(admins.get(caller))) { return null; };
        let allUsers = Buffer.Buffer<(Text, Text, Nat)>(0);

        for ((id, cust) in customers.entries()) {
            let balance = await icrc1_balance_of({ owner = id; subaccount = null });
            allUsers.add(("Customer", cust.name, balance));
        };
        
        for ((id, org) in organizers.entries()) {
            let balance = await icrc1_balance_of({ owner = id; subaccount = null });
            allUsers.add(("Organizer", org.name, balance));
        };
        
        for ((id, admin) in admins.entries()) {
            let balance = await icrc1_balance_of({ owner = id; subaccount = null });
            allUsers.add(("Admin", admin.name, balance));
        };
        ?Buffer.toArray(allUsers)
    };

    public shared ({ caller }) func createConcert(name: Text, date: Time.Time, totalTickets: Nat, price: Nat) : async Text {
        if (Option.isNull(organizers.get(caller))) { return "Error: Only organizers can create concerts"; };
        let id = "concert-" # Nat.toText(concerts.size() + 1);
        let concert = { id; name; date; totalTickets; soldTickets = 0; price; organizerId = caller; };
        concerts.put(id, concert);
        let organizer = organizers.get(caller);
        switch (organizer) {
            case (?org) { 
                let updatedConcerts = Array.append(org.concerts, [id]); 
                let updatedOrg = { id = org.id; name = org.name; concerts = updatedConcerts; revenue = org.revenue }; 
                organizers.put(caller, updatedOrg); 
            };
            case null {};
        };
        return id;
    };

    private func getAdminPrincipal() : ?Principal {
        for ((id, admin) in admins.entries()) {
            return ?id;
        };
        null
    };

    public shared ({ caller }) func buyTicket(concertId: Text) : async Text {
        let concertOpt = concerts.get(concertId);
        switch (concertOpt) {
            case (?concert) {
                let currentTime = Time.now();
                let oneDayInNanoseconds = 24 * 60 * 60 * 1_000_000_000;
                if (concert.date <= currentTime + oneDayInNanoseconds) {
                    return "Error: Cannot buy ticket for a concert that is today or in the past";
                };
                if (concert.soldTickets >= concert.totalTickets) { return "Error: Sold out"; };
                let priceInTokens = concert.price * (10 ** Nat8.toNat(init.decimals));
                let platformFee = (priceInTokens * platformFeePercentage) / 100;
                assert(priceInTokens >= platformFee);
                let organizerAmount : Nat = priceInTokens - platformFee;
                let customerAccount: Account = { owner = caller; subaccount = null };
                let customerBalance = balance(customerAccount, log);
                let totalRequired = priceInTokens + (2 * init.transfer_fee);
                if (customerBalance < totalRequired) {
                    return "Error: Insufficient balance";
                };
                let adminPrincipal = getAdminPrincipal();
                let adminAccount: Account = switch (adminPrincipal) {
                    case (?principal) { { owner = principal; subaccount = null } };
                    case null { return "Error: No admin found"; };
                };
                let adminTransfer : Transfer = {
                    spender = customerAccount;
                    source = #Icrc1Transfer;
                    from = customerAccount;
                    to = adminAccount;
                    amount = platformFee;
                    fee = ?init.transfer_fee;
                    memo = null;
                    created_at_time = null;
                };
                switch (applyTransfer(adminTransfer)) {
                    case (#Err(err)) { return "Error: Admin fee transfer failed - " # debug_show(err); };
                    case (#Ok(_)) {};
                };
                let organizerAccount: Account = { owner = concert.organizerId; subaccount = null };
                let organizerTransfer : Transfer = {
                    spender = customerAccount;
                    source = #Icrc1Transfer;
                    from = customerAccount;
                    to = organizerAccount;
                    amount = organizerAmount;
                    fee = ?init.transfer_fee;
                    memo = null;
                    created_at_time = null;
                };
                switch (applyTransfer(organizerTransfer)) {
                    case (#Err(err)) {
                        let refundTransfer : Transfer = {
                            spender = adminAccount;
                            source = #Icrc1Transfer;
                            from = adminAccount;
                            to = customerAccount;
                            amount = platformFee;
                            fee = ?init.transfer_fee;
                            memo = null;
                            created_at_time = null;
                        };
                        ignore applyTransfer(refundTransfer);
                        return "Error: Organizer payment failed - " # debug_show(err);
                    };
                    case (#Ok(_)) {};
                };
                platformRevenue += platformFee;
                switch (organizers.get(concert.organizerId)) {
                    case (?org) {
                        let updatedOrg = { id = org.id; name = org.name; concerts = org.concerts; revenue = org.revenue + organizerAmount };
                        organizers.put(concert.organizerId, updatedOrg);
                    };
                    case null { return "Error: Organizer not found"; };
                };
                let tokenId = "ticket-" # concertId # "-" # Nat.toText(concert.soldTickets + 1);
                let metadata = [
                    ("concert_name", concert.name),
                    ("date", Int.toText(concert.date / 1_000_000)),
                    ("organizer", Principal.toText(concert.organizerId))
                ];
                let nft = { tokenId; concertId; owner = caller; metadata; used = false };
                nfts.put(tokenId, nft);
                switch (customers.get(caller)) {
                    case (?cust) { 
                        let updatedTickets = Array.append(cust.tickets, [tokenId]); 
                        let updatedCust = { id = cust.id; name = cust.name; tickets = updatedTickets }; 
                        customers.put(caller, updatedCust); 
                    };
                    case null {};
                };
                let updatedConcert = { 
                    id = concert.id; name = concert.name; date = concert.date; 
                    totalTickets = concert.totalTickets; soldTickets = concert.soldTickets + 1; 
                    price = concert.price; organizerId = concert.organizerId 
                };
                concerts.put(concertId, updatedConcert);
                return tokenId;
            };
            case null { return "Error: Concert not found"; };
        };
    };

    public shared ({ caller }) func validateTicket(tokenId: Text) : async Text {
        let nftOpt = nfts.get(tokenId);
        switch (nftOpt) {
            case (?nft) {
                let concertOpt = concerts.get(nft.concertId);
                switch (concertOpt) {
                    case (?concert) {
                        if (concert.organizerId != caller) { return "Error: Only organizer can validate"; };
                        let currentTime = Time.now();
                        if (concert.date < currentTime) {
                            let updatedNFT = { tokenId = nft.tokenId; concertId = nft.concertId; owner = nft.owner; metadata = nft.metadata; used = true };
                            nfts.put(tokenId, updatedNFT);
                            return "Error: Concert date passed";
                        };
                        if (nft.used) { return "Error: Ticket already used"; };
                        let updatedNFT = { tokenId = nft.tokenId; concertId = nft.concertId; owner = nft.owner; metadata = nft.metadata; used = true };
                        nfts.put(tokenId, updatedNFT);
                        return "Ticket validated successfully";
                    };
                    case null { return "Error: Concert not found"; };
                };
            };
            case null { return "Error: Ticket not found"; };
        };
    };

    public query func icrc7_name() : async Text { "Medley Concert Tickets"; };
    public query func icrc7_symbol() : async Text { "MCT"; };
    public query func icrc7_token_metadata(tokenId: Text) : async ?[(Text, Text)] {
        switch (nfts.get(tokenId)) { case (?nft) { ?nft.metadata }; case null { null }; };
    };
    public query func icrc7_owner_of(tokenId: Text) : async ?Text {
        switch (nfts.get(tokenId)) { case (?nft) { ?Principal.toText(nft.owner) }; case null { null }; };
    };
    public query func icrc7_balance_of(owner: Principal) : async Nat {
        switch (customers.get(owner)) { case (?cust) { cust.tickets.size() }; case null { 0 }; };
    };
    public query func icrc7_tokens_of(owner: Principal) : async [Text] {
        switch (customers.get(owner)) { case (?cust) { cust.tickets }; case null { [] }; };
    };
    public query func icrc7_supported_standards() : async [{ name: Text; url: Text }] {
        [{ name = "ICRC-7"; url = "https://github.com/dfinity/ICRC-1/tree/main/standards/ICRC-7" }];
    };

    public query func getConcerts(search: Text, minPrice: Nat, maxPrice: Nat, onlyAvailable: Bool) : async [Concert] {
        var allConcerts = Iter.toArray(concerts.vals());
        if (search != "") { allConcerts := Array.filter(allConcerts, func (c: Concert) : Bool { Text.contains(c.name, #text search) }); };
        if (minPrice > 0 and maxPrice > 0) { allConcerts := Array.filter(allConcerts, func (c: Concert) : Bool { c.price >= minPrice and c.price <= maxPrice }); }
        else if (minPrice > 0) { allConcerts := Array.filter(allConcerts, func (c: Concert) : Bool { c.price >= minPrice }); }
        else if (maxPrice > 0) { allConcerts := Array.filter(allConcerts, func (c: Concert) : Bool { c.price <= maxPrice }); };
        if (onlyAvailable) { allConcerts := Array.filter(allConcerts, func (c: Concert) : Bool { c.soldTickets < c.totalTickets }); };
        allConcerts
    };

    public query func getConcert(id: Text) : async (Concert, Bool) {
        let concertOpt = concerts.get(id);
        switch (concertOpt) { case (?concert) { return (concert, true); }; case null { return (emptyConcert, false); }; };
    };

    public query func getCustomerTickets(caller: Principal) : async [Ticket] {
        let customer = customers.get(caller);
        switch (customer) {
            case (?cust) { 
                Array.map(cust.tickets, func (tokenId: Text) : Ticket {
                    switch (nfts.get(tokenId)) {
                        case (?nft) { 
                            let concertOpt = concerts.get(nft.concertId);
                            switch (concertOpt) {
                                case (?concert) {
                                    let currentTime = Time.now();
                                    let isValid = if (concert.date < currentTime) {
                                        let updatedNFT = { tokenId = nft.tokenId; concertId = nft.concertId; owner = nft.owner; metadata = nft.metadata; used = true };
                                        nfts.put(tokenId, updatedNFT);
                                        false
                                    } else { not nft.used };
                                    { id = nft.tokenId; concertId = nft.concertId; owner = nft.owner; isValid }
                                };
                                case null { { id = tokenId; concertId = ""; owner = caller; isValid = false } };
                            }
                        };
                        case null { { id = tokenId; concertId = ""; owner = caller; isValid = false } };
                    }
                })
            };
            case null { [] };
        };
    };

    public query func getOrganizerRevenue(caller: Principal) : async Nat {
        switch (organizers.get(caller)) {
            case (?org) { org.revenue };
            case null { 0 };
        };
    };

    public query func getOrganizerConcerts(caller: Principal) : async [Text] {
        let organizer = organizers.get(caller);
        switch (organizer) { case (?org) { return org.concerts; }; case null { return []; }; };
    };

    public shared ({ caller }) func editConcert(concertId: Text, name: Text, date: Time.Time, totalTickets: Nat, price: Nat) : async Bool {
        let concertOpt = concerts.get(concertId);
        switch (concertOpt) {
            case (?concert) {
                if (concert.organizerId != caller) { return false; };
                if (concert.soldTickets > 0) { return false; };
                let updatedConcert = { id = concert.id; name; date; totalTickets; soldTickets = concert.soldTickets; price; organizerId = concert.organizerId };
                concerts.put(concertId, updatedConcert);
                return true;
            };
            case null { return false; };
        };
    };

    public shared ({ caller }) func deleteConcert(concertId: Text) : async Bool {
        let concertOpt = concerts.get(concertId);
        switch (concertOpt) {
            case (?concert) {
                if (concert.organizerId != caller) { return false; };
                if (concert.soldTickets > 0) { return false; };
                concerts.delete(concertId);
                let organizer = organizers.get(caller);
                switch (organizer) {
                    case (?org) { 
                        let updatedConcerts = Array.filter(org.concerts, func (cid: Text) : Bool { cid != concertId }); 
                        let updatedOrg = { id = org.id; name = org.name; concerts = updatedConcerts; revenue = org.revenue }; 
                        organizers.put(caller, updatedOrg); 
                    };
                    case null {};
                };
                return true;
            };
            case null { return false; };
        };
    };
    
    public query func getPlatformRevenue() : async Nat { platformRevenue };
};