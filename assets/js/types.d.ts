type ElmMsg = PlaySoundElmMsg | LogErrorElmMsg

interface PlaySoundElmMsg {
  type: "playSound"
  data: {
    pitch: number, voiceIndex: number, soundId: string, volume: number
  }
}

interface LogErrorElmMsg {
  type: "logError",
  data: {
    message: string
  }
}

interface ElmApp {
  ports: {
    sendPortMessage: {
      subscribe: (callback: (msg: ElmMsg) => void) => void
    }
    receivePortMessage: {
      send: (msg: ElmMsg) => void
    }
  }
}
