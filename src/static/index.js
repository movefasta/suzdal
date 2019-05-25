'use strict';

// const IpfsClient = require('ipfs-http-client');
// const OrbitDB = require('orbit-db');
// const ipfs = IpfsClient('localhost', '5001');

// (async () => {
//     const orbitdb = await OrbitDB.createInstance(ipfs)
//     const db = await orbitdb.log('hello')
// })();

const { Elm } = require('../elm/Main');

var storageKey = "suzdal";
var flags = localStorage.getItem(storageKey);

console.log("Retrieved state: ", flags);


var app = Elm.Main.init({
    flags: flags
});

// Evan's example
// var storedState = localStorage.getItem('elm-todo-save');
// var startingState = storedState ? JSON.parse(storedState) : null;
// var app = Elm.Main.init({ flags: startingState });
// app.ports.setStorage.subscribe(function(state) {
//     localStorage.setItem('elm-todo-save', JSON.stringify(state));
// });

app.ports.storePeers.subscribe(function(val) {
    localStorage.setItem("peers", JSON.stringify(val));
});

app.ports.fetchPeers.subscribe(function() {
    var peers = localStorage.getItem('peers');
    setTimeout(function() { 
        app.ports.getPeers.send(peers);
    }, 200);
});



app.ports.setStorage.subscribe(function(val) {
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