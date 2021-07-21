import './style/index.css';
import { Elm } from './elm/Main.elm';
import firebase from "firebase/app";
require("firebase/firestore");

const app = Elm.Main.init({
  node: document.getElementById('root')
});

const firebaseConfig = {
  apiKey: "AIzaSyDZ_1pXpe4QKqNDRLrJ2L2LFDgV-zIXzEI",
  authDomain: "jan-log.firebaseapp.com",
  projectId: "jan-log",
  storageBucket: "jan-log.appspot.com",
  messagingSenderId: "794370952",
  appId: "1:794370952:web:d3e43fe296fee4b417d03d",
  measurementId: "G-CHL94CR1KZ"
};

firebase.initializeApp(firebaseConfig);

const db = firebase.firestore();

const res = db.collection("rounds").get().then((querySnapshot) => {
  querySnapshot.forEach((doc) => {
    console.log(`${doc.id} => ${doc.data().players}`);
    console.log(JSON.stringify(doc.data().players));
    return doc.data().players[0];
    });
});

app.ports.read.send(String(res));
