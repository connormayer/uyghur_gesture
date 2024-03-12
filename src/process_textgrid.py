import csv
import re
import tgt

textgrid_file = "data/audio/uyghur_conversation.TextGrid"
textgrid = tgt.read_textgrid(textgrid_file)

ipa_to_uyghur_3 = {
	'a': 'a',
	'b': 'b',
	'd': 'd',
	'e': 'é',
	'f': 'f',
	'h': 'h',
	'i': 'i',
	'k': 'k',
	'l': 'l',
	'm': 'm',
	'n': 'n',
	'o': 'o',
	'p': 'p',
	'q': 'q',
	'r': 'r',
	's': 's',
	't': 't',
	'u': 'u',
	'w': 'w',
	'ø': 'ö',
	'ŋ': 'ng',
	'ɛ': 'e',
	'ɡ': 'g',
	'ʁ': 'gh',
	'ʃ': 'sh',
	'ʒ': 'zh',
	'χ': 'x'
}

ipa_to_uyghur_2 = {
	'd͡ʒ': 'j',
	't͡ʃ': 'ch'
}

ipa_to_uyghur_1 = {
	'j': 'y'
}

ipa_to_uyghur_0 = {
	'y': 'ü'
}

def ipa_to_uyg(word):
	new_word = word
	for key, val in ipa_to_uyghur_0.items():
		new_word = re.sub(key, val, new_word)

	for key, val in ipa_to_uyghur_1.items():
		new_word = re.sub(key, val, new_word)

	for key, val in ipa_to_uyghur_2.items():
		new_word = re.sub(key, val, new_word)

	for key, val in ipa_to_uyghur_3.items():
		new_word = re.sub(key, val, new_word)

	return new_word

rows = []
headers = [
	'subject',
	'word',
	'syllable',
	'start_time',
	'end_time',
	'word_len_syls',
	'syl_position',
	'closed_syl',
	'is_initial',
	'is_final',
	'is_penultimate',
	'is_first_heavy',
	'is_last_heavy',
	'preceding_heavy',
	'following_heavy',
	'gesture'
]

def check_heavy(syl):
	return not syl[-1] in ('a', 'e', 'i', 'o', 'u', 'ü', 'ö', 'é')

for tier_name in textgrid.get_tier_names():
	if 'syllables' in tier_name:
		speaker_prefix = tier_name.split('-')[0].strip()
		tier = textgrid.get_tier_by_name(tier_name)
		word_tier_name = '{} - words'.format(speaker_prefix)
		word_tier = textgrid.get_tier_by_name(word_tier_name)

		for interval in tier:
			# Get properties of syllable
			start_time = interval.start_time
			end_time = interval.end_time
			midpoint = start_time + (end_time - start_time) / 2
			label = interval.text

			# Get containing word
			word = word_tier.get_annotations_by_time(midpoint)[-1].text
			uyg_label = ipa_to_uyg(label)
			syllables = [re.sub("'", "", x) for x in word.split('.')]
			word_len = len(syllables)
			try:
				syl_position = syllables.index(uyg_label)
			except:
				print(word)
				print(uyg_label)
				breakpoint()
			closed_syl = check_heavy(uyg_label)
			is_initial = syl_position == 0
			is_final = syl_position == (word_len - 1)
			is_penultimate = syl_position == (word_len - 2)
			is_first_heavy = closed_syl and all(not check_heavy(x) for x in syllables[0:syl_position])
			is_last_heavy = closed_syl and all(not check_heavy(x) for x in syllables[syl_position + 1:])
			preceding_heavy = any(check_heavy(x) for x in syllables[0:syl_position])
			following_heavy = any(check_heavy(x) for x in syllables[syl_position+1:])

			rows.append([
				speaker_prefix,
				word,
				label,
				start_time,
				end_time,
				word_len,
				syl_position,
				closed_syl,
				is_initial,
				is_final,
				is_penultimate,
				is_first_heavy,
				is_last_heavy,
				preceding_heavy,
				following_heavy,
				0
			])

with open('data/syllable_data.csv', 'w') as f:
	writer = csv.writer(f)
	writer.writerow(headers)
	writer.writerows(rows)
