buildmac:
	@stack build --ghc-options=-I/Library/Developer/CommandLineTools/SDKs/MacOSX12.1.sdk/usr/include/ffi

testmac:
	@stack test --ghc-options=-I/Library/Developer/CommandLineTools/SDKs/MacOSX12.1.sdk/usr/include/ffi

run:
	@stack run 
