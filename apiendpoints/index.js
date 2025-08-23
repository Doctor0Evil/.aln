const express = require("express");
const app = express();
app.use(express.json());

app.post("/log", (req, res) => {
  res.status(200).json({ status: "logged" });
});

app.post("/saveplayer", (req, res) => {
  res.status(200).json({ saved: true });
});

app.get("/loadplayer", (req, res) => {
  res.status(200).json({ player: { username: "Alice", ... } });
});

app.get("/item/:id", (req, res) => {
});

app.get("/items", (req, res) => {
 
});

module.exports = app;
