import "./style/index.css";
import { Elm } from "./elm/Main.elm";

import firebase from "firebase/app";
import "firebase/firestore";

const app = Elm.Main.init({
  node: document.getElementById("root"),
});
const firebaseConfig = {
  apiKey: "AIzaSyDZ_1pXpe4QKqNDRLrJ2L2LFDgV-zIXzEI",
  authDomain: "jan-log.firebaseapp.com",
  projectId: "jan-log",
  storageBucket: "jan-log.appspot.com",
  messagingSenderId: "794370952",
  appId: "1:794370952:web:d3e43fe296fee4b417d03d",
  measurementId: "G-CHL94CR1KZ",
};
firebase.initializeApp(firebaseConfig);
const db = firebase.firestore();

app.ports.fetchLog.subscribe((logId) => {
  db.collection("logs")
    .doc(logId)
    .get()
    .then((doc) => {
      if (doc.exists) {
        app.ports.fetchedLog.send({ ...doc.data(), logId });
      } else {
        app.ports.fetchedLogButNoLog.send(null);
      }
    });
});

app.ports.updateLog.subscribe((dto) => {
  db.collection("logs").doc(dto.logId).set(dto);
});

app.ports.listenLog.subscribe((logId) => {
  db.doc(`logs/${logId}`).onSnapshot((doc) => {
    if (doc.exists) {
      app.ports.fetchedLog.send({ ...doc.data(), logId });
    }
  });
});
