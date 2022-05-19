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
import sf2_guitar from './sf2/0251_Acoustic_guitar'

/** @type any */
const win = window

const AudioContextFunc = win.AudioContext || win.webkitAudioContext;
const audioContext = new AudioContextFunc();
const player = new WebAudioFontPlayer();

function $(selector) {
  return Array.prototype.slice.apply(document.querySelectorAll(selector))
}

topbar.config({barColors: {0: "#29d"}, shadowColor: "rgba(0, 0, 0, .3)"})
topbar.show()
const promises = [
  player.adjustPreset(audioContext, sf2_guitar)
]
Promise.all(promises).then(() => {
  topbar.progress(1)
  topbar.hide()
  console.timeEnd("adjustPreset")
  var gainDrums = audioContext.createGain();gainDrums.connect(audioContext.destination);gainDrums.gain.value=0.5;
  var gainSynth = audioContext.createGain();gainSynth.connect(audioContext.destination);gainSynth.gain.value=0.3;
  var gainBass = audioContext.createGain();gainBass.connect(audioContext.destination);gainBass.gain.value=0.7;
  var gainHit = audioContext.createGain();gainHit.connect(audioContext.destination);gainHit.gain.value=0.5;
  function playNote(gain, preset, pitch, duration) {
    player.playNote(audioContext, gain, preset, pitch, duration);
  }

  function playGuiar(pitch, duration) {
    playNote(gainBass, sf2_guitar, pitch, duration);
  }

  const playVoiceBtns = $("[data-role=play-voice]")
  playVoiceBtns.map((button) => button.addEventListener("mousedown", downListener))
  playVoiceBtns.map((button) => button.addEventListener("mouseover", overListener))


  let csrfToken = document.querySelector("meta[name='csrf-token']").getAttribute("content")
  let liveSocket = new LiveSocket("/live", Socket, {params: {_csrf_token: csrfToken}})
  let socket = new Socket("/socket", {params: {token: win.userToken}})


  liveSocket.connect()
  socket.connect()

  function pushUpdate(event) {
    const pitch = parseInt(event.target.dataset.pitch);
    const voice_index = parseInt(event.target.dataset.voiceIndex);
    channel.push("update_instrument", {pitch, voice_index})
  }

  function downListener(event) {
    pushUpdate(event)
  }
  function overListener(event) {
    if(event.buttons > 0) {
      pushUpdate(event)
    }
  }
  channel.on("update_instrument", payload => {
    playGuiar(payload.pitch, 2)
  })
})
