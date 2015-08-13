CLASH = clash
topLevel = Main
part = XC6SLX9-TQG144-2

ucfFiles = ..\\vhdl\\extra\\constraints.ucf

srcDir = Z:\\git\\Clash\\examples\\MOS6502
working = working

# coregen_work_dir ?= ./coregen-tmp
map_opts = -timing -ol high -detail -pr b -register_duplication -w
par_opts = -ol high
isedir = C:\\Xilinx\\14.7\\ISE_DS
xil_env = $(isedir)\\settings64.bat


# flashsize ?= 8192

sshPreCmd = cmd /c Z: & cd $(srcDir)\\$(working) & $(xil_env)

SHELL = /bin/zsh

# test: 
# 	 echo '$(addprefix -uc , $(ucfFiles))'

all : $(working)/$(topLevel).bit

junk += $(working)

# Create a project file
$(working)/$(topLevel).prj: vhdl/$(topLevel)/$(topLevel).vhdl Makefile
	test -d $(working) || mkdir $(working)
	for src in vhdl/$(topLevel)/*.vhdl; do echo "vhdl work ../$$src" >> $(working)/$(topLevel).tmpprj; done
	echo "vhdl work ../vhdl/extra/ipcores/clkwiz50.vhd" >> $(working)/$(topLevel).tmpprj
	sort -u $(working)/$(topLevel).tmpprj > $(working)/$(topLevel).prj
	rm -f $(working)/$(topLevel).tmpprj

# and the xst script file
$(working)/$(topLevel).scr: Makefile 
	test -d $(working) || mkdir $(working)
	echo "run" > $@
	echo "-p $(part)" >> $@
	echo "-top $(topLevel)" >> $@
	echo "-ifn $(topLevel).prj" >> $@
	echo "-ofn $(topLevel).ngc" >> $@

# create the net list file
$(working)/$(topLevel).ngc: vhdl/$(topLevel)/$(topLevel).vhdl $(working)/$(topLevel).scr $(working)/$(topLevel).prj
	echo $(isedir)
	ssh administrator@10.211.55.3  '$(sshPreCmd) & xst -ifn $(topLevel).scr'

# Xilinx version of the netlist
$(working)/$(topLevel).ngd: $(working)/$(topLevel).ngc
	ssh administrator@10.211.55.3 '$(sshPreCmd) & ngdbuild $(addprefix -uc , $(ucfFiles)) $(topLevel).ngc'

# Map
$(working)/$(topLevel).ncd: $(working)/$(topLevel).ngd
	ssh administrator@10.211.55.3 '$(sshPreCmd) & map $(map_opts) $(topLevel).ngd'

#Par
$(working)/$(topLevel)_par.ncd: $(working)/$(topLevel).ncd
	ssh administrator@10.211.55.3 '$(sshPreCmd) & par $(par_opts) -w $(topLevel).ncd $(topLevel)_par.ncd'


#Bitgen
$(working)/$(topLevel).bit: $(working)/$(topLevel)_par.ncd
	$(xil_env); \
	ssh administrator@10.211.55.3 '$(sshPreCmd) & bitgen -g DriveDone:yes -g StartupClk:Cclk -w $(topLevel)_par.ncd $(topLevel).bit'



# Create the VHDL files
vhdl/$(topLevel)/$(topLevel).vhdl : $(topLevel).hs Makefile
	rm -rf vhdl/$(topLevel)
	clash -O -odir obj -hidir obj --vhdl $(topLevel)
junk += vhdl/$(topLevel)
junk += obj


clean :
	rm -rf $(junk)



