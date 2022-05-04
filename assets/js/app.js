// We import the CSS which is extracted to its own file by esbuild.
// Remove this line if you add a your own CSS build pipeline (e.g postcss).
import "../css/app.css"

// If you want to use Phoenix channels, run `mix help phx.gen.channel`
// to get started and then uncomment the line below.
import {channel} from "./user_socket.js"

// You can include dependencies in two ways.
//
// The simplest option is to put them in assets/vendor and
// import them using relative paths:
//
//     import "../vendor/some-package.js"
//
// Alternatively, you can `npm install some-package --prefix assets` and import
// them using a path starting with the package name:
//
//     import "some-package"
//


// Include phoenix_html to handle method=PUT/DELETE in forms and buttons.
import "phoenix_html"
// Establish Phoenix Socket and LiveView configuration.
import {Socket} from "phoenix"
import {LiveSocket} from "phoenix_live_view"
import topbar from "../vendor/topbar"
import {WebAudioFontPlayer} from "../vendor/WebAudioFontPlayer"
import sf2_0480_Chaos from './sf2/0480_Chaos'
import sf2_0390_Aspirin from './sf2/0390_Aspirin'
import {C3, G3, C4, C5, d5} from './notes'

var AudioContextFunc = window.AudioContext || window.webkitAudioContext;
var audioContext = new AudioContextFunc();
var player = new WebAudioFontPlayer();

topbar.config({barColors: {0: "#29d"}, shadowColor: "rgba(0, 0, 0, .3)"})
topbar.show()
console.time("adjustPreset")
const promises = [
  player.adjustPreset(audioContext, sf2_0480_Chaos),
  player.adjustPreset(audioContext, sf2_0390_Aspirin)
]
Promise.all(promises).then(() => {
  topbar.progress(1)
  topbar.hide()
  console.timeEnd("adjustPreset")
  var gainDrums = audioContext.createGain();gainDrums.connect(audioContext.destination);gainDrums.gain.value=0.5;
  var gainSynth = audioContext.createGain();gainSynth.connect(audioContext.destination);gainSynth.gain.value=0.3;
  var gainBass = audioContext.createGain();gainBass.connect(audioContext.destination);gainBass.gain.value=0.7;
  var gainHit = audioContext.createGain();gainHit.connect(audioContext.destination);gainHit.gain.value=0.5;
  var bpm = 120;
  var N = 4 * 60 / bpm;
  // function orchestra(pitch, duration){return {gain:gainHit,preset:_tone_0550_Chaos_sf2_file,pitch:pitch,duration:duration*N};}
  function synth(pitch, duration){return {gain:gainSynth,preset:sf2_0480_Chaos,pitch:pitch,duration:duration*N};}
  function bass(pitch, duration){return {gain:gainBass,preset:sf2_0390_Aspirin,pitch:pitch,duration:duration*N};}
  // function drum(){return {gain:gainDrums,preset:_drum_36_6_JCLive_sf2_file,pitch:36,duration:1};}
  // function snare(){return {gain:gainDrums,preset:_drum_40_6_JCLive_sf2_file,pitch:38,duration:1};}
  // function hihat(){return {gain:gainDrums,preset:_drum_42_6_JCLive_sf2_file,pitch:42,duration:1};}
  // function open(){return {gain:gainDrums,preset:_drum_46_6_JCLive_sf2_file,pitch:46,duration:1};}
  function playNote(gain, preset, pitch, duration) {
    player.playNote(audioContext, gain, preset, pitch, duration);
  }

  function playBass(pitch, duration) {
    playNote(gainBass, sf2_0390_Aspirin, pitch, duration);
  }

  function play() {
    playBass(C4);
  }
  function stop() {
    player.stop(audioContext, gainBass)
  }

  document.querySelector("#button").addEventListener("mousedown", play)
  document.querySelector("#button").addEventListener("mouseup", stop)




  // let csrfToken = document.querySelector("meta[name='csrf-token']").getAttribute("content")
  // let liveSocket = new LiveSocket("/live", Socket, {params: {_csrf_token: csrfToken}})
  // let socket = new Socket("/socket", {params: {token: window.userToken}})

  // let button         = document.querySelector("#button")
  // let paragraph         = document.querySelector("#paragraph")

  // // Show progress bar on live navigation and form submits
  // topbar.config({barColors: {0: "#29d"}, shadowColor: "rgba(0, 0, 0, .3)"})

  // // connect if there are any LiveViews on the page
  // liveSocket.connect()
  // socket.connect()

  // // expose liveSocket on window for web console debug logs and latency simulation:
  // // >> liveSocket.enableDebug()
  // // >> liveSocket.enableLatencySim(1000)  // enabled for duration of browser session
  // // >> liveSocket.disableLatencySim()
  // window.liveSocket = liveSocket

  // button.addEventListener("mousedown", downListener)
  // button.addEventListener("mouseup", upListener)
  // button.addEventListener("mouseout", upListener)

  // function downListener() {
  //   channel.push("update_instrument", {body: {down: true}})
  //   console.log("sent down")
  // }
  // function upListener() {
  //   channel.push("update_instrument", {body: {down: false}})
  //   console.log("sent up")
  // }
  // channel.on("update_instrument", payload => {
  //   console.log("received down")
  //   if(payload.body.down) {
  //     paragraph.innerHTML = "button is pressed!"
  //   } else {
  //     paragraph.innerHTML = "button is NOT pressed"
  //   }
  // })
})
