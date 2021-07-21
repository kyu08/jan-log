import "./style/index.css";
import { Elm } from "./elm/Main.elm";
import firebase from "firebase/app";
require("firebase/firestore");

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

const res = db
  .collection("rounds")
  .get()
  .then((querySnapshot) => {
    querySnapshot.forEach((doc) => {
      // console.log(`${doc.id} => ${doc.data().players}`);
      // console.log(JSON.stringify(doc.data().players));
      return doc.data().players[0];
    });
  });

const defaultRound4 = { data0: 0, data1: 0, data2: 0, data3: 0 };
const defaultLog4 = (logId) => {
  return {
    logId,
    gameFee: 0,
    rate: 100,
    chipRate: 2,
    players: ["player1", "player2", "player3", "player4"],
    rounds: Array(4).fill(defaultRound4),
    chips: [0, 0, 0, 0],
  };
};

app.ports.fetchLog.subscribe((logId) => {
  db.collection("logs")
    .get(logId)
    .then((querySnapshot) =>
      querySnapshot.forEach((doc) => {
        const docData = doc.data();
        const res = !docData ? defaultLog4(logId) : { ...docData, logId };
        app.ports.fetchedLog.send(res);
      })
    );
});

app.ports.updateLog.subscribe(
  ({ logId, gameFee, rate, chipRate, players, rounds, chips }) => {
    db.collection("logs").doc(logId).set({
      gameFee,
      rate,
      chipRate,
      players,
      rounds,
      chips,
    });
  }
);
