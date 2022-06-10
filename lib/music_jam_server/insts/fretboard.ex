defmodule MusicJamServer.Insts.Fretboard do
  def json(octave \\ 3) do
    interpolate = &MusicJamServer.MusicNotes.interpolate/2
    to_scalar = &MusicJamServer.MusicNotes.to_scalar/1

    Enum.map(
      [
        interpolate.(to_scalar.({:E, octave + 2}), to_scalar.({:E, octave + 4})),
        interpolate.(to_scalar.({:B, octave + 1}), to_scalar.({:B, octave + 3})),
        interpolate.(to_scalar.({:G, octave + 1}), to_scalar.({:G, octave + 3})),
        interpolate.(to_scalar.({:D, octave + 1}), to_scalar.({:D, octave + 3})),
        interpolate.(to_scalar.({:A, octave}), to_scalar.({:A, octave + 2})),
        interpolate.(to_scalar.({:E, octave}), to_scalar.({:E, octave + 2}))
      ],
      fn notes ->
        %{
          currentPitch: 0,
          currentVolume: 0,
          lastNoteStartTime: 0,
          notes: notes
        }
      end
    )
  end

  def width do
    2000
  end

  def height do
    200
  end

  def fret_count do
    24
  end

  def fret_distance(fret_number) do
    e = 2.718
    k = 5.71584144995393e-2
    width() * 1.3 * (1 - e ** (-k * fret_number))
  end

  def fret_width(fret_number) do
    fret_distance(fret_number + 1) - fret_distance(fret_number)
  end

  def viewbox do
    Enum.join([0, 0, width(), height() + 20], " ")
  end

  def exterior_points do
    Enum.join(
      Enum.map([[0, 0], [width(), 0], [width(), height()], [0, height()], [0, 0]], fn p ->
        Enum.join(p, ",")
      end),
      " "
    )
  end

  def frets_path do
    for fret_index <- 1..fret_count() do
      "M" <> Enum.join([fret_distance(fret_index), 0, fret_distance(fret_index), height()], " ")
    end
  end

  def string_path(string_index, note_index, _playing, _time) do
    y = (string_index + 1) * 28
    "M #{fret_distance(note_index)} #{y} L #{fret_distance(note_index + 1)} #{y}"
  end

  def is_inlay_fret?(fret_number) do
    # a little weird with inlay on first "fret" because that "fret" affords the open string in the current design
    Enum.find([3, 5, 7, 9, 0], fn n -> n == rem(fret_number + 1, 12) - 1 end) != nil
  end
end
