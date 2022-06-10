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

interface AppStateChangeElmMsg {
  type: "appStateChange",
  data: {
    sleeping: boolean;
  }
}

interface ElmApp {
  ports: {
    outbox: {
      subscribe: (callback: (msg: ElmMsg) => void) => void
    }
    inbox: {
      send: (msg: ElmMsg) => void
    }
  }
}

