import ara_to_lat
import re
import syllabifier
import tgt
import os

# Fix ng issue

simple_ortho_map = {
	'ng': 'N',
	'sh': 'S',
	'ch': 'C',
	'gh': 'G',
	'zh': 'Z'
}

def convert_ortho(word, reverse = False):
	for key, val in simple_ortho_map.items():
		if reverse:
			word = re.sub(val, key, word)
		else:
			word = re.sub(key, val, word)
	return word

textgrid_files = [
	#"data/textgrids_to_syllabify/5004_5005_CONGEST_UYGHUR_INTERVIEW.TextGrid",
	#'data/textgrids_to_syllabify/5006_5007_CONGEST_UYGHUR_INTERVIEW_INPUT1.TextGrid',
	#"data/textgrids_to_syllabify/5006_5007_CONGEST_UYGHUR_INTERVIEW_INPUT2.TextGrid", # DONE
	'data/textgrids_to_syllabify/5008_5009_CONGEST_UYGHUR_INTERVIEW_INPUT1.TextGrid',
	#"data/textgrids_to_syllabify/5008_5009_CONGEST_UYGHUR_INTERVIEW_INPUT2.TextGrid",
	#"data/textgrids_to_syllabify/uyghur_conversation.TextGrid", # DONE
]
for textgrid_file in textgrid_files:
	# Praat seems to randomly alternate between using utf-16 and utf-8...
	try:
		textgrid = tgt.read_textgrid(textgrid_file, encoding='utf-16')
	except:
		textgrid = tgt.read_textgrid(textgrid_file, encoding='utf-8')

	transducer = ara_to_lat.load_transducer()
	syller = syllabifier.get_syllabifier()

	for tier_name in textgrid.get_tier_names():
		if 'words' in tier_name:
			# prefix = tier_name.split('-')[0].strip()
			# syl_tier = tgt.core.IntervalTier(name = '{} - syllables'.format(prefix))
			syl_tier = tgt.core.IntervalTier(name = 'syllables')
			tier_pos = textgrid.get_tier_names().index(tier_name)
			textgrid.insert_tier(syl_tier, tier_pos + 1)

			tier = textgrid.get_tier_by_name(tier_name)

			for interval in tier:
				# Convert interval text to Latin
				interval.text = ara_to_lat.convert_word(transducer, interval.text)

				if '?' in interval.text or interval.text in ('laughter', 'spn'):
					continue

				# Syllabify word
				simple_word = convert_ortho(interval.text)
				simple_syllabified = syller.val_b(simple_word)
				if not simple_syllabified:
					breakpoint()
					print("Can't parse: {}".format(simple_word))
					continue
				else:
					simple_syllabified = simple_syllabified[0]

				interval.text = convert_ortho(
					simple_syllabified, reverse = True
				)

				# phone_tier_name = '{} - phones'.format(prefix)
				phone_tier_name = 'phones'
				phone_tier = textgrid.get_tier_by_name(phone_tier_name)
				phone_intervals = phone_tier.get_annotations_between_timepoints(
					interval.start_time - 0.01, interval.end_time + 0.01
				)
				if len(phone_intervals) != len(re.sub("['-]", "", simple_word)):
					breakpoint()
					print("Mismatched number of intervals: {}".format(simple_word))
					continue
				
				start_time = phone_intervals[0].start_time
				cur_idx = 0
				syl_str = ''
				for segment in simple_syllabified:
					if segment == '.':
						end_time = phone_intervals[cur_idx - 1].end_time
						interval = tgt.core.Interval(start_time, end_time, syl_str)
						syl_tier.add_interval(interval)
						start_time = end_time
						syl_str = ''
					elif segment == "'" or segment == "-":
						pass
					else:
						syl_str += phone_intervals[cur_idx].text
						cur_idx += 1

				end_time = phone_intervals[-1].end_time
				interval = tgt.core.Interval(start_time, end_time, syl_str)
				syl_tier.add_interval(interval)

		elif 'utterances' in tier_name:
			tier = textgrid.get_tier_by_name(tier_name)
			for interval in tier:
				try:
					interval.text = ara_to_lat.convert_sentence(transducer, interval.text)
				except:
					breakpoint()
					print("Couldn't parse interval {}".format(interval.text))
	outname = os.path.splitext(textgrid_file)[0] + '_syllabified.TextGrid'
	tgt.write_to_file(textgrid, outname)
