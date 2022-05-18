defmodule MusicJamServer.MusicNotesTest do
  use ExUnit.Case

  test "interpolate" do
    expected = [
      f: 4,
      G: 4,
      g: 4,
      A: 4,
      a: 4,
      B: 4,
      C: 5,
      c: 5,
      D: 5,
      d: 5,
      E: 5,
      F: 5,
      f: 5,
      G: 5,
      g: 5,
      A: 5,
      a: 5,
      B: 5,
      C: 6,
      c: 6,
      D: 6,
      d: 6,
      E: 6,
      F: 6,
      f: 6,
      G: 6,
      g: 6,
      A: 6,
      a: 6,
      B: 6,
      C: 7,
      c: 7,
      D: 7,
      d: 7,
      E: 7,
      F: 7,
      f: 7,
      G: 7,
      g: 7,
      A: 7,
      ]
    assert MusicJamServer.MusicNotes.interpolate({:f, 4}, {:A, 7}) === expected
  end
end
