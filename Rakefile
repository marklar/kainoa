require 'rake/clean'

srcdir = 'Kainoa'
outputdir = 'obj'
bindir = 'bin'
o_name = 'readFromFile'
['.'].each do |dir|
  CLEAN.include("#{outputdir}/#{dir}/*.hi", "#{outputdir}/#{dir}/*.o")
end
CLOBBER.include("#{bindir}/#{o_name}")

# -keep-hc-files 
task :default do
  `ghc -XScopedTypeVariables --make #{o_name}.hs -outputdir #{outputdir} -o #{bindir}/#{o_name}`
end
