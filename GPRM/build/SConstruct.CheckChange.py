""" 
This SCons file simply detects that the src file has changed and reports it 
"""

src_file=ARGUMENTS.get('SRC','NONE')
src_file_name = src_file.split('/').pop()
# without the target, this does not work!
target_file = '._CHECK_CHANGE_.'+src_file_name
env = Environment()
cmd='echo $SOURCE > $TARGET;  echo 1'
env.Command(target_file, src_file, cmd)
