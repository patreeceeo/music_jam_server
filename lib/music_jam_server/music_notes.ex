defmodule MusicJamServer.MusicNotes do
  @type note_name :: :C | :c | :D | :d | :E | :F | :f | :G | :g | :A | :a | :B
  @type note :: { note_name(), integer() }

  @spec list_names() :: list(note_name())
  def list_names do
    [
      :C,
      :c,
      :D,
      :d,
      :E,
      :F,
      :f,
      :G,
      :g,
      :A,
      :a,
      :B,
    ]
  end

  @spec find_name_index(note_name()) :: integer()
  def find_name_index(needle) do
    Enum.find_index(list_names(), fn hay -> needle == hay end)
  end

  @spec interpolate(note(), note(), acc :: list(note())) :: list(note())
  def interpolate(first, last, acc \\ []) do
    first_scalar = to_scalar(first)
    last_scalar = to_scalar(last)
    if first_scalar > last_scalar do
        raise "MusicNotes.interpolate: first arg should be lower pitch"
    end
    if first_scalar < last_scalar do
        Enum.concat([first], interpolate(from_scalar(first_scalar + 1), last, acc))
    else
        [last]
    end
  end

  @spec from_scalar(integer()) :: note()
  def from_scalar(scalar) do
    names = list_names()
    note_name = Enum.at(names, rem(scalar, 12))
    octave = floor(scalar / 12)
    {note_name, octave}
  end

  @spec to_scalar(note()) :: integer()
  def to_scalar({note_name, octave}) do
    index = find_name_index(note_name)
    index + octave * 12
  end
end
