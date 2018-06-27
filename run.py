import argparse
import subprocess


# Compile and run haskell program
run_ghc = 'stack ghc Main.hs'
copy_to_build = 'mv *.hi *.o Main ../build'
run_haskell = './Main'

subprocess.Popen(run_ghc, stdout=subprocess.PIPE, cwd='ML/src/', shell=True).communicate()
subprocess.Popen(copy_to_build, stdout=subprocess.PIPE, cwd='ML/src/', shell=True).communicate()
subprocess.Popen(run_haskell, stdout=subprocess.PIPE, cwd='ML/build/', shell=True).communicate()

# Copy out.data to clojure project
subprocess.Popen('mv data/out.data ../plotting/data/', stdout=subprocess.PIPE, cwd='ML', shell=True).communicate()

# Compile and generate chart
compile_jar = 'lein uberjar'
run_jar = 'java -jar target/plotting-0.1.0-SNAPSHOT-standalone.jar out.data output'

subprocess.Popen(compile_jar, cwd='plotting',stdout=subprocess.PIPE, shell=True).communicate()
subprocess.Popen(run_jar, cwd='plotting',stdout=subprocess.PIPE, shell=True).communicate()