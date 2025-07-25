import apertium
import argparse
import hfst
import io
import pandas as pd
import os
import re
import time

# FST_PATH = "/mnt/e/git_repos/apertium-uig/dev/ortho/ara-lat.hfst"
FST_PATH = "/home/connor/git_repos/apertium-uig/dev/ortho/ara-lat.hfst"

def load_transducer():
	"""
	Loads the orthographic fst we use to convert Perso-Arabic to Latin
	""" 
	istr = hfst.HfstInputStream(FST_PATH)
	transducer = istr.read_all()[0]
	transducer.lookup_optimize()
	return transducer

def convert_word(transducer, word):
	if not word or pd.isna(word):
		return ''

	parses = transducer.lookup(word)
	if parses:
		return parses[1][0]
	else:
		return word

def convert_sentence(transducer, sentence):
	if not sentence or pd.isna(sentence):
		return ''

	words = sentence.split(' ')
	parsed_words = [convert_word(transducer, x) for x in words]
	return ' '.join(parsed_words)

if __name__ == "__main__":
	data = pd.read_csv('uyghur_gesture_youtube.csv')
	transducer = load_transducer(fst_path)
	data['label_wd'] = data.label_wd.apply(lambda x: convert_word(transducer, x))
	data['label_st'] = data.label_st.apply(lambda x: convert_sentence(transducer, x))
	data.to_csv('uyghur_gesture_youtube_lat.csv')