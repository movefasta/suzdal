'use strict';

require('./assets/images/loading.svg');

const { Elm } = require('../elm/Main');

var app = Elm.Main.init({
    flags: localStorage.getItem('suzdal')
});

initLocalStoragePort(app);

const storage = window.localStorage || {
  setItem(k, v) {
    this[k] = v;
  },
  getItem(k) {
    return this[k];
  }
};

function initLocalStoragePort(elmApp) {
  elmApp.ports.storeObject.subscribe(function ([key, state]) {
    storeObject(key, state);
    console.log("Stored Object: ", state);
  });
  elmApp.ports.retrieveObject.subscribe(function (key) {
    const o = retrieveObject(key);
    elmApp.ports.objectRetrieved.send([key, o]);
    console.log("Retrieve Object with key: ", [key, o]);
  });
};

function storeObject(key, object) {
  storage.setItem(key, JSON.stringify(object));
};

function retrieveObject(key) {
  const value = storage.getItem(key);
  return value ? JSON.parse(value) : null;
};
