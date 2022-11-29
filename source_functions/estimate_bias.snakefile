# snakemake -s source_functions/estimate_bias.snakefile -j 400 --rerun-incomplete --latency-wait 30 --config --cluster-config source_functions/cluster_config/estimate_bias.cluster.json --cluster "sbatch -p {cluster.p} -o {cluster.o} --account {cluster.account} -t {cluster.t} -c {cluster.c} --mem {cluster.mem} --account {cluster.account} --mail-user {cluster.mail-user} --mail-type {cluster.mail-type}" -p &> log/snakemake_log/estimate_bias/210113.estimate_bias.log

import os

configfile: "source_functions/config/estimate_bias.config.yaml"

# Make log directories if they don't exist
os.makedirs("log/slurm_out/estimate_bias", exist_ok = True)
for x in expand("log/slurm_out/estimate_bias/{rules}", rules = config['rules']):
	os.makedirs(x, exist_ok = True)

os.makedirs("log/psrecord/estimate_bias", exist_ok = True)
os.makedirs("log/psrecord/estimate_bias/airemlf90", exist_ok = True)

rule all:
	input: expand("data/derived_data/estimate_bias/{model}/{iter}/airemlf90.{model}.{iter}.log", model = config['model'], iter = config['iter']), expand("data/derived_data/estimate_bias/{model}/{iter}/cleaned.txt", model = config['model'], iter = config['iter'])

rule setup_data:
	input:
		# Data from full AIREML run for specified model
		full_dat = "data/derived_data/aireml_varcomp/{model}/data.txt",
		script = "source_functions/setup.estimate_bias.R",
		genotyped = "data/derived_data/grm_inbreeding/mizzou_hairshed.diagonal.full_reg.csv",
		full_ped = "data/derived_data/3gen/full_ped.rds"
	params:
		r_module = config['r_module'],
		model = "{model}",
		iter = "{iter}",
		pct_excl = config['pct_excl'],
		hair_col = lambda wildcards: expand("{hair_col}", hair_col = config['hair_col'][wildcards.model]),
	output:
		subset_dat = "data/derived_data/estimate_bias/{model}/{iter}/data.txt",
		ped = "data/derived_data/estimate_bias/{model}/{iter}/ped.txt",
		pull_list = "data/derived_data/estimate_bias/{model}/{iter}/pull_list.txt"
	shell:
		"""
		module load {params.r_module}
		Rscript --vanilla {input.script} {params.model} {params.iter} {params.pct_excl} {params.hair_col}
		"""

rule pull_genotypes:
	input:
		pull_list = "data/derived_data/estimate_bias/{model}/{iter}/pull_list.txt",
		master_geno = config['geno_prefix'] + ".fwf.txt"
	output:
		reduced_geno = "data/derived_data/estimate_bias/{model}/{iter}/genotypes.txt"
	shell:
	# https://www.gnu.org/software/gawk/manual/html_node/Printf-Examples.html
		"""
		grep -Fwf {input.pull_list} {input.master_geno} | awk '{{printf "%-25s %s\\n", $1, $2}}' > {output.reduced_geno}
		"""

rule copy_par:
	input:
		par = "source_functions/par/aireml_varcomp.{model}.par",
	output:
		par = "data/derived_data/estimate_bias/{model}/{iter}/estimate_bias.par",
	# Replace map file path in original parameter file
	# Can't use absolute path: renumf90 has a character limit and it gets cut off
	shell:
		"""
		grep -v "OPTION chrinfo" {input.par} > {output.par}
		echo "OPTION chrinfo ../../../../raw_data/geno_dump/200924_HairShed.850K.chrinfo.txt" >> {output.par}
		"""

rule renumf90:
	input:
		input_par = "data/derived_data/estimate_bias/{model}/{iter}/estimate_bias.par",
		reduced_geno = "data/derived_data/estimate_bias/{model}/{iter}/genotypes.txt",
		subset_dat = "data/derived_data/estimate_bias/{model}/{iter}/data.txt",
		ped = "data/derived_data/estimate_bias/{model}/{iter}/ped.txt",
		map = config['geno_prefix'] + '.chrinfo.txt'
	params:
		dir = "data/derived_data/estimate_bias/{model}/{iter}",
		renumf90_path = config['renumf90_path'],
		renf90_in_name = "estimate_bias.par",
		renf90_out_name = "renf90.{model}.{iter}.out"
	output:
		renf90_par = "data/derived_data/estimate_bias/{model}/{iter}/renf90.par",
		renf90_tables = "data/derived_data/estimate_bias/{model}/{iter}/renf90.tables",
		renf90_dat = "data/derived_data/estimate_bias/{model}/{iter}/renf90.dat"
	shell:
		"""
		cd {params.dir}
		{params.renumf90_path} {params.renf90_in_name} &> {params.renf90_out_name}
		"""

rule airemlf90:
	input:
		renf90_par = "data/derived_data/estimate_bias/{model}/{iter}/renf90.par",
		renf90_tables = "data/derived_data/estimate_bias/{model}/{iter}/renf90.tables",
		renf90_dat = "data/derived_data/estimate_bias/{model}/{iter}/renf90.dat",
		map = config['geno_prefix'] + '.chrinfo.txt'
	params:
		dir = "data/derived_data/estimate_bias/{model}/{iter}",
		aireml_out_name = "aireml.{model}.{iter}.out",
		aireml_log = "airemlf90.{model}.{iter}.log",
		aireml_path = config['aireml_path'],
		psrecord = "/storage/hpc/group/UMAG/WORKING/hjdzpd/mizzou_hairshed/log/psrecord/estimate_bias/airemlf90/airemlf90.{model}.{iter}.psrecord",
		mpi_module = config['mpi_module']
	output:
		aireml_solutions = "data/derived_data/estimate_bias/{model}/{iter}/solutions",
		aireml_log = "data/derived_data/estimate_bias/{model}/{iter}/airemlf90.{model}.{iter}.log"
	shell:
		"""
		export OMPI_MCA_btl_openib_if_include='mlx5_3:1'
		module load {params.mpi_module}
		cd {params.dir}
		psrecord "{params.aireml_path} renf90.par &> {params.aireml_out_name}" --log {params.psrecord} --include-children --interval 2
		mv airemlf90.log {params.aireml_log}
		"""

# Remove genotype files to save space
rule cleanup:
	input:
		aireml_log = "data/derived_data/estimate_bias/{model}/{iter}/airemlf90.{model}.{iter}.log",
		solutions = "data/derived_data/estimate_bias/{model}/{iter}/solutions"
	params:
		dir = "data/derived_data/estimate_bias/{model}/{iter}"
	output:
		cleaned = "data/derived_data/estimate_bias/{model}/{iter}/cleaned.txt"
	shell:
		"""
		rm {params.dir}/genotypes.txt*
		echo 'done' > {output.cleaned}
		"""
