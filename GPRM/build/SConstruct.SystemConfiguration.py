""" \file SConstruct
   
 \brief GPRM - SCons script for building SystemConfiguration for C++ GPRM & SystemC
"""

# $Id$

import os
import re
#import commands
import subprocess
import sys
#sys.path+=['/usr/lib/scons/']
ext = ''
if subprocess.check_output(['uname',''], shell=True).strip()  == "Darwin": 
    ext='-darwin'
else:
    ext = '-linux'

ymlpath=ARGUMENTS.get('Y','.')
lib_path=ARGUMENTS.get('D','.')
sba_dir=ARGUMENTS.get('WD','.')

#src_file='SBA/SystemConfiguration.rb'
target_file = lib_path+'/'+'SystemConfiguration.h'
#script='./Cxx_SystemConfigurationOO.pl'
script = os.environ['GPRM_DIR']+'/bin/genSysConfig'+ext
src_file=ymlpath # HACK! 

env = Environment()

#cmd='GPRM_DIR='+os.environ['GPRM_DIR'] +' /usr/bin/perl '+script+' '+' -Y '+ymlpath+' -D '+lib_path+' -C '+sba_dir
cmd= 'GPRM_DIR='+os.environ['GPRM_DIR'] +' '+script+' '+' -Y '+ymlpath+' -D '+lib_path+' -C '+sba_dir
#print cmd
gen=env.Command(target_file, src_file, cmd)
env.Alias('gen',target_file)
env.Depends(gen,script)
env.Depends(gen,ymlpath)
