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

const defaultRound4 = {
  points: { data0: 0, data1: 0, data2: 0, data3: 0 },
  chicha: null,
};
const defaultLog4 = {
  gameFee: 0,
  rate: 50,
  chipRate: 2,
  players: ["player1", "player2", "player3", "player4"],
  rounds: Array(4).fill(defaultRound4),
  chips: [0, 0, 0, 0],
  rankPoint: [10, 20],
  havePoint: 25,
  returnPoint: 30,
};

app.ports.fetchLog.subscribe((logId) => {
  db.collection("logs")
    .doc(logId)
    .get()
    .then((doc) => {
      const res = doc.exists
        ? { ...doc.data(), logId }
        : { ...defaultLog4, logId };
      app.ports.fetchedLog.send(res);
    });
});

app.ports.updateLog.subscribe(
  ({
    logId,
    gameFee,
    rate,
    chipRate,
    players,
    rounds,
    chips,
    rankPoint,
    havePoint,
    returnPoint,
  }) => {
    db.collection("logs").doc(logId).set({
      gameFee,
      rate,
      chipRate,
      players,
      rounds,
      chips,
      rankPoint,
      havePoint,
      returnPoint,
    });
  }
);

app.ports.listenLog.subscribe((logId) => {
  db.doc(`logs/${logId}`).onSnapshot((doc) => {
    const res = doc.exists
      ? { ...doc.data(), logId }
      : { ...defaultLog4, logId };
    app.ports.listenedLog.send(res);
  });
});
