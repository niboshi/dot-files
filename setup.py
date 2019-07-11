#!/usr/bin/env python3

import os
import sys
import argparse
import shutil

THIS_DIR = os.path.dirname(os.path.realpath(__file__))


FILE_LIST = [
	(".emacs.d", None),
	(".tmux.conf", None),
	(".bashrc", None),
	(".bash_profile", None),
	(".profile", None),
	(".vimrc", None),
	(".gitconfig", None),
	(".gitignore", None),
	(".inputrc", None),
	(".ssh_rc", ".ssh/rc"),
	("bin", ".bindirs/dot-files"),
]

def do_link(src_path, dst_path, force=False):
	if os.path.islink(dst_path):
		os.remove(dst_path)
	elif os.path.exists(dst_path):
		print("Destination file exists: {}".format(dst_path))
		if force:
			cancel = False
		else:
			while True:
				ans = input("Overwrite(y/n)? ")
				ans = ans.lower()
				if ans in ('y', 'n'):
					break
			cancel = (ans == 'n')

		if cancel:
			return False

		if os.path.isdir(dst_path):
			shutil.rmtree(dst_path)
		else:
			os.remove(dst_path)

	os.symlink(src_path, dst_path)
	return True

def main(arglist):
	parser = argparse.ArgumentParser()
	parser.add_argument('-y', action='store_true', dest='force')
	args = parser.parse_args(arglist)

	home_dir = os.path.expanduser("~")

	# Create bindir
	if not os.path.isdir(os.path.join(home_dir, ".bindirs")):
		os.mkdir(os.path.join(home_dir, ".bindirs"))

	# Create links
	for src, dst in FILE_LIST:
		if dst is None:
			dst = src
		src_path = os.path.join(THIS_DIR, src)
		dst_path = os.path.join(home_dir, dst)
		assert os.path.exists(src_path), src_path
		do_link(src_path, dst_path, force=args.force)
	pass

if __name__ == '__main__':
	main(sys.argv[1:])
