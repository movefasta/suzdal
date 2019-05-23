'use strict';

const IpfsClient = require('ipfs-http-client');
const OrbitDB = require('orbit-db');
const ipfs = IpfsClient('localhost', '5001');
const { Elm } = require('../elm/Main');

var storageKey = "suzdal";
var flags = localStorage.getItem(storageKey);

console.log("Retrieved state: ", flags);

var app = Elm.Main.init({
    flags: flags
});


(async () => {
    const orbitdb = await OrbitDB.createInstance(ipfs)
    const db = await orbitdb.log('hello')
})();


app.ports.storeCache.subscribe(function(val) {
    if (val === null) {
        localStorage.removeItem(storageKey);
    } else {
        localStorage.setItem(storageKey, JSON.stringify(val));
    }

    // Report that the new session was stored succesfully.
    // setTimeout(function() { app.ports.onStoreChange.send(val); }, 0);
});

// Whenever localStorage changes in another tab, report it if necessary.
window.addEventListener("storage", function(event) {
    if (event.storageArea === localStorage && event.key === storageKey) {
        app.ports.onStoreChange.send(event.newValue);
    }
}, false);