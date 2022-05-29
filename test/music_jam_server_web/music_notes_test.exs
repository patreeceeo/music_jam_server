defmodule MusicJamServer.MusicNotesTest do
  use ExUnit.Case

  test "interpolate" do
    expected = Enum.to_list(1..12)
    assert MusicJamServer.MusicNotes.interpolate(1, 12) === expected
  end
end
