'''\file SConstruct.test.py

    \brief Gannet SBA - SCons script for building Gannet

'''

##   (c) 2004-2009 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>

# $Id$


###############################################################################
# DON'T MODIFY ANYTHING BELOW THIS
###############################################################################

import os
import re
import commands
import sys
#sys.path+=['/usr/lib/scons/']

from SCons.Variables import Variables
from SCons.Environment import Environment
#from SCons.Help import Help

#from cxxtestgenlib import createRunner


def build(wd,sources):

    destdir='../SBA/'
    global opts

    flags=[]
    switches=[]
    boost=0

    # MACROS
    wordsz=64
    WORDSZ='WORDSZ='+str(wordsz)
    GL='GPRM_LANGUAGE'
    NEW=''
    VERBOSE='' #V='VERBOSE'
    # Compile for VM (otherwise compiles to model HW)
    SEQVM='SEQVM=0'
    USE_THREADS='USE_THREADS=0'
    DISTR='DISTR=0'
    use_pthreads = False
    # B ashkan
    USE_TILERA='' #X= 'USE_TILERA'
    use_tilera = False
    USE_MIC=''
    use_mic = False
    # E ashkan
    STEAL=''
    KERNEL_HAS_STATE=''
    KERNEL_LOCK=''
    THREADED_CORE='' # 'THREADED_CORE=0'
    threaded_core = False
    # Count CPU cycles
    CYCLES='' # 'CYCLES'
    TIMINGS='' # 'TIMINGS'
    STATIC_ALLOC='STATIC_ALLOC'

    # Flags
    #Smash= '-fstack-protector '# Ashkan
    WARN='' #'-Wall '
    CXX0X = '-std=c++11'
    PTHREADS = '-pthread' 
    OPTSPEED    = '-O3 -fno-exceptions -fno-rtti '
    OPTSIZE = '-Os -fno-exceptions -fno-rtti '
    OPTTHREADS = '-O2 ' # '-ansi-alias '
    DEBUG = ''
    ARCH=''
    NO_SOCKET=''
    OPT = '-O2 '
    CYGWIN=0
    PIC = '' #'-fPIC '
    # These are used by the build script to generate flags/switches
    OSX=0
    # Flag for cross-compilation
    XC=0
    # Use LLVM
    # LLVM=1: x86, LLVM=2: ppc
    LLVM=0
    # SystemC
    SC=0
    H=0 # Help
    LIB=False
    SHLIB=False
    MACROS = [] # ['INTERFACE_OBJ']
    yaml_config='../../SystemConfigurations/SBA.yml'

    #use options without leading '-': scons v=0 gui=QtGui
    opts = Variables()
    opts.Add('v', 'Verbose', 0)
    opts.Add('w', 'Warnings', 0)
    opts.Add('new', 'New', 0)
    opts.Add('xc', 'Crosscompile','NONE')
    opts.Add('llvm', 'Use LLVM',0)
    opts.Add('win','CygWin',0)
    opts.Add('vm', 'Virtual Machine',0)
    opts.Add('sock', 'Use POSIX socket interface',1)
    opts.Add('svm', 'Sequential Virtual Machine',0)
# options can't take . or / in the strings!!
#    opts.Add('yml','YAML configuration file','') #'../../SBA.yml')
    opts.Add('cycles', 'Count CPU cycles',0)
    opts.Add('timings', 'Time program execution',0)
    opts.Add('dyn', 'Dynamic memory',0)
    opts.Add('pthreads', 'Use POSIX Threads',0)
    opts.Add('lib', 'Compile as a library named gannet ',0)
    opts.Add('shlib', 'Compile as a shared library named gannet ',0)
    opts.Add('steal', 'Enable task stealing',0)
    opts.Add('stateful', 'For stateful kernels',0)    
    opts.Add('locking', 'Always lock access to stateful kernels',0)    
    opts.Add('wordsz', 'Set WORDSZ',64)
    opts.Add('ptcore', 'Use POSIX Threaded Core',0)    
    opts.Add('dbg', 'Debug',0)
    opts.Add('nogen',"Don't generate C++ sources from Ruby code",0) 
    opts.Add('opt', 'Optimise','speed') # or 'size'
    opts.Add('D','Macros (add as a string: D="MACRO1:1 MACRO2 MACRO3:whatever"\nSCons is too stupid to allow "=")','') # add additional macros as a string
    opts.Add('h', 'Help',0)

    args=sys.argv[1:]
    #import getopt
    #rest = getopt.getopt(args,"hABCD")
    for arg in args:
        if re.match("(\w+)=(\w+)",arg):
            (k,v)=arg.split('=')
            opts.args[k]=v

    #exit(opts.options)

    for param in os.environ.keys():
        if param == "VERBOSE":
            VERBOSE='VERBOSE'
        if param == "GPRM_YML_CONFIG":
            yaml_config=os.environ["GPRM_YML_CONFIG"]

    for option in opts.options:
        if option.key == 'v' and opts.args.has_key(option.key) and opts.args[option.key]!=option.default:
            VERBOSE='VERBOSE'
        if option.key == 'w' and opts.args.has_key(option.key) and opts.args[option.key]!=option.default:
            WARN='-Wall '
        if option.key == 'new' and opts.args.has_key(option.key) and opts.args[option.key]!=option.default:
            NEW='NEW=1'
        if option.key == 'xc' and opts.args.has_key(option.key) and opts.args[option.key]!=option.default:
            XC=1
            # B ashkan
            if opts.args[option.key] == 'Tilera':
                use_tilera=True
                USE_TILERA='USE_TILERA'
            elif opts.args[option.key] == 'MIC':
                use_mic=True
                USE_MIC='USE_MIC'
                '-ansi-alias '
            # E ashkan
            OPT=OPTSPEED
        if option.key == 'llvm' and opts.args.has_key(option.key): # and opts.args[option.key]!=option.default:
            if opts.args[option.key]=='1':
                LLVM=1
            elif opts.args[option.key]=='2':
                LLVM=2
            else:
                LLVM=0
            OPT=OPTSPEED
        if option.key == 'win' and opts.args.has_key(option.key) and opts.args[option.key]!=option.default:
            CYGWIN=1
        if option.key == 'vm' and opts.args.has_key(option.key) and opts.args[option.key]!=option.default:
            VM='VM=1'
        if option.key == 'svm' and opts.args.has_key(option.key) and opts.args[option.key]!=option.default:
            VM='VM=1'
            SEQVM='SEQVM=1'
# doesn't work if the path has dots or slashes!
#        if option.key == 'yml' and opts.args.has_key(option.key) and opts.args[option.key]!=option.default:
#            print "YAML!"
#            yaml_config=opts.args[option.key]
        if option.key == 'sock' and opts.args.has_key(option.key) and opts.args[option.key]!=option.default:
            NO_SOCKET='NO_SOCKET'
            sockpatt=re.compile('^\.\.\/GannetSocket')
            nsources=filter(lambda s: not(sockpatt.search(s)),sources)
            sources=nsources
        if option.key == 'wordsz' and opts.args.has_key(option.key) and opts.args[option.key]!=option.default:
            wordsz=opts.args[option.key]
            WORDSZ='WORDSZ='+str(wordsz)
        if option.key == 'lib' and opts.args.has_key(option.key) and opts.args[option.key]!=option.default:
            LIB=True
        if option.key == 'shlib' and opts.args.has_key(option.key) and opts.args[option.key]!=option.default:
            SHLIB=True
        if option.key == 'steal' and opts.args.has_key(option.key) and opts.args[option.key]!=option.default:
            STEAL='STEAL=1'
        if option.key == 'stateful' and opts.args.has_key(option.key) and opts.args[option.key]!=option.default:
            KERNEL_HAS_STATE='KERNEL_HAS_STATE=1'            
        if option.key == 'locking' and opts.args.has_key(option.key) and opts.args[option.key]!=option.default:
            KERNEL_LOCK='KERNEL_LOCK=1'            
        if option.key == 'pthreads' and opts.args.has_key(option.key) and opts.args[option.key]!=option.default:
            USE_THREADS='USE_THREADS=1'
            use_pthreads=True
            OPT=OPTTHREADS
        if option.key == 'ptcore' and opts.args.has_key(option.key) and opts.args[option.key]!=option.default:
            THREADED_CORE='THREADED_CORE=1'
            threaded_core=True
            OPT=OPTTHREADS
        if option.key == 'distr' and opts.args.has_key(option.key) and opts.args[option.key]!=option.default:
            DISTR='DISTR=1'
            OPT=OPTSPEED
        if option.key == 'cycles' and opts.args.has_key(option.key) and opts.args[option.key]!=option.default:
            CYCLES='CYCLES'
        if option.key == 'timings' and opts.args.has_key(option.key) and opts.args[option.key]!=option.default:
            TIMINGS='TIMINGS'
        if option.key == 'dyn' and opts.args.has_key(option.key) and opts.args[option.key]!=option.default:
            STATIC_ALLOC=''
        if option.key == 'dbg' and opts.args.has_key(option.key) and opts.args[option.key]!=option.default:
            DEBUG='-g -O0 ' #'-g -fno-exceptions -fno-rtti '
            OPT=''
        if option.key == 'opt' and opts.args.has_key(option.key) and opts.args[option.key]!=option.default:
            OPT=OPTSPEED
        if option.key == 'D' and  opts.args.has_key(option.key) and opts.args[option.key]!=option.default:
            macrostr=re.sub('\s*:\s*','=',opts.args[option.key])
            MACROS=macrostr.split(' ')
        if option.key == 'h' and opts.args.has_key(option.key):
            H=1

    if commands.getoutput("uname") == "Darwin":
        OSX=1
        switches+=['DARWIN']
        PTHREADS = ''
        if SC==1:
            ARCH='-arch i386'
            
    # Ashkan commented this out       
    #if XC==1:
    #    switches.append('__ppc__')

    FLAGS=''
    SWITCHES=''
    flags+=[CXX0X,WARN,DEBUG,ARCH,PIC,OPT,PTHREADS]
    # Ashkan added USE_TILERA
    switches+=[VERBOSE,SEQVM,WORDSZ,CYCLES,TIMINGS,STATIC_ALLOC,NO_SOCKET,USE_THREADS,THREADED_CORE,DISTR, STEAL, KERNEL_HAS_STATE, KERNEL_LOCK, USE_TILERA, USE_MIC]+MACROS
    for flag in flags:
        if flag !='':
            FLAGS+=flag+' '
            

    for switch in switches:
        if re.search('BOOST',switch):
            boost=1
        if switch != '':
            SWITCHES+='-D'+switch+' '

    GPRM_DIR=os.environ["GPRM_DIR"]
#    print "GPRM_DIR:"+GPRM_DIR
 			
    bin='gannetvm'+str(wordsz)
    if LIB:
       sources.append(GPRM_DIR+'/GPRM/src/gannet.cc')
    else:
       sources.append(GPRM_DIR+'/GPRM/src/gannetvm.cc')
    #------------------------------------------------------------------------------
    cxx= os.environ['CXX']

#    gcc_version = os.popen('g++ -dumpversion').read().rstrip("\n\r")
#    gcc_version_tuple = [int(x) for x in gcc_version.split('.')]
## WV: HACK! TOO SPECIFIC! USE ENV VAR!
#    if (gcc_version_tuple[1]<6):
#        cxx='/opt/local/bin/g++-mp-4.8'
#    else:
#        cxx='g++'
##Ashkan" none of them, icpc!
#	cxx='/home/ashkan/intel/composer_xe_2013_sp1.2.144/bin/intel64/icpc -no-offload -vec-report6'
        
    if XC==1:
       # cxx='powerpc-405-linux-gnu-g++'
       if use_tilera: 
#         cxx='tile-c++'
          cxx='tile-g++' 
       elif use_mic:
          cxx='/home/ashkan/intel/composer_xe_2013_sp1.2.144/bin/intel64/icpc -mmic -no-offload -vec-report6'
       else:   
          print "WARNING: NO VALID CROSS COMPILATION TARGET, using host CXX"  
    if LLVM==1:
        cxx='i686-pc-linux-gnu-g++'

    env = Environment(variables = opts, CXX = cxx, CXXFLAGS = FLAGS+SWITCHES, LINKFLAGS = [PTHREADS])
#    env.VariantDir(wd+'build/',GPRM_DIR+'/GPRM/build/', duplicate=0)    	
#   Help(opts.GenerateHelpText(env))
    if H==1:
        print(opts.GenerateHelpText(env))
        exit(1)

    if XC==1:
        HOME=os.environ['HOME']
        env['ENV']['PATH']=os.environ["GPRM_XC_PATH"]+":"+env['ENV']['PATH']
        print env['ENV']['PATH']

    if LLVM==1:
        env['ENV']['PATH']=os.environ["GPRM_LLVM_PATH"]+":"+env['ENV']['PATH']
        print env['ENV']['PATH']
 
#FIXME: dl only needed for dynamic loading!
#libs=['m','dl'] 
    libs=['m'] 
    if use_pthreads or threaded_core:
        libs+=['pthread']
    # Ashkan added tmc    
    if use_tilera:
        libs+=[]
#        libs+=['tmc']
#libs+=['pthread','boost_thread-mt']

    # SBA classes should not use boost shared libraries, only header files
    if boost==1:
        libs+=['boost_program_options']

    INCpaths=['.','../',GPRM_DIR+'/GPRM/src/SBA/'] # ,GPRM_DIR+'/GPRM/SBA/ServiceCoreLibraries/']
    LIBpaths=[]

    if boost==1:
        INCpaths+=[os.environ['BOOST_INC']]

    if OSX==1 or OSX==0: # UGLY HACK!
        INCpaths=[wd+'/gensrc/',wd+'/src/GPRM/Kernel/',wd+'/src/','.','../',GPRM_DIR+'/GPRM/src/SBA/',GPRM_DIR+'/GPRM/src/']
#        LIBpaths=['/opt/local/lib/gcc48/','/opt/local/lib/','/usr/local/lib/']
#        libs=['m']
        if boost==1:
            INCpaths+=[os.environ['BOOST_INC']]
            libs+=[os.environ['BOOST_LIB']] 

    #WV: to have multiple targets, we just need to set bin : bin is short for
    #env.Program(target=bin,...)

    if LIB:
            glib=env.Library('gannet',sources,LIBS=libs,LIBPATH=LIBpaths,CPPPATH=INCpaths)
    elif SHLIB:
            glib=env.SharedLibrary('gannet',sources,LIBS=libs,LIBPATH=LIBpaths,CPPPATH=INCpaths)
    else:
            prog=env.Program(bin,sources,LIBS=libs,LIBPATH=LIBpaths,CPPPATH=INCpaths)
            env.Install(wd+'/bin',prog)
            env.Alias('install',wd+'/bin') # make this wd+'/bin' ?


