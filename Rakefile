require 'rake/clean'

srcdir = 'Kainoa'
outputdir = 'obj'
bindir = 'bin'
o_name = 'search'
['hi', 'o'].each do |ext|
  CLEAN.include("#{outputdir}/**/*.#{ext}")
end
CLOBBER.include("#{bindir}/#{o_name}")

# -keep-hc-files 
task :default do
  `ghc -XScopedTypeVariables --make #{o_name}.hs -threaded -outputdir #{outputdir} -o #{bindir}/#{o_name}`
end
