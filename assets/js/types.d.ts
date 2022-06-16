type ElmMsg = PlayNoteElmMsg | LogErrorElmMsg

interface PlayNoteElmMsg {
  type: "playNote"
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

