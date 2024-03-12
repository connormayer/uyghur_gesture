sound_id = selected("Sound")
textgrid_id = selected("TextGrid")

selectObject: sound_id
pitch_id = To Pitch: 0.0, 75.0, 600.0

selectObject: textgrid_id
n_intervals = Get number of intervals: 1
Insert point tier: 2, "f0"

for i from 1 to n_intervals
label$ = Get label of interval: 1, i
if length(label$)
start_interval = Get starting point: 1, i
end_interval = Get end point: 1, i
selectObject:pitch_id
maximum_f0 = Get time of maximum: start_interval, end_interval, "Hertz", "Parabolic"
selectObject: textgrid_id
nocheck Insert point: 2, maximum_f0, "H"
endif
endfor

selectObject: sound_id, textgrid_id
View & Edit

