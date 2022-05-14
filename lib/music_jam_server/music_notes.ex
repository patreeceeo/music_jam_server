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

  @spec get_pitch(note()) :: integer()
  def get_pitch({note_name, octave}) do
    (octave * 12) + find_name_index(note_name)
  end

  @spec compare(note(), note()) :: integer()
  def compare({a_name, a_octave}, {b_name, b_octave}) do
    a_scalar = find_name_index(a_name) + a_octave * 12
    b_scalar = find_name_index(b_name) + b_octave * 12
    if(a_scalar > b_scalar) do
      -1
    else
      if (a_scalar === b_scalar) do
        0
      else
        1
      end
    end
  end

  @spec next_note(note()) :: note()
  def next_note({note_name, octave}) do
    index = find_name_index(note_name)
    names = list_names()
    if(index === length(names) - 1) do
      { Enum.at(names, 0), octave + 1 }
    else
      { Enum.at(names, index + 1), octave }
    end
  end

  @spec interpolate(note(), note(), acc :: list(note())) :: list(note())
  def interpolate(from, to, acc \\ []) do
    case compare(from, to) do
      -1 ->
        raise "MusicNotes.interpolate: first arg should be lower pitch"
      1 ->
        Enum.concat([from], interpolate(next_note(from), to, acc))
      0 ->
        [to]
    end
  end
end
