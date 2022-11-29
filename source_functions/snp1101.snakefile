# Command I used to run this
# snakemake -s source_functions/snp1101.snakefile -j 400 --rerun-incomplete --latency-wait 30 --config --cluster-config source_functions/cluster_config/snp1101.cluster.json --cluster "sbatch -p {cluster.p} -o {cluster.o} --account {cluster.account} -t {cluster.t} -c {cluster.c} --mem {cluster.mem} --account {cluster.account} --mail-user {cluster.mail-user} --mail-type {cluster.mail-type}" -p &> log/snakemake_log/snp1101/210106.snp1101.log

import os

configfile: "source_functions/config/snp1101.config.yaml"

# Make log directories if they don't exist
os.makedirs("log/slurm_out/snp1101", exist_ok = True)
for x in expand("log/slurm_out/snp1101/{rules}", rules = config['rules']):
	os.makedirs(x, exist_ok = True)

rule all:
	input: expand("data/derived_data/snp1101/{model}/out/gwas_ssr_{model}_bvs_p.txt", model = config['model'])

# Calls setup.snp1101.R to create a trait file for SNP1101 given a solutions file and pedigree file calculated in BLUPF90 and stored at {dir}
# Trait file is created by de-regressing breeding values calculated by BLUPF90
# animal_effect is whatever effect BLUPF90 assigned the random animal effect
rule setup_snp1101:
	input:
		solutions = lambda wildcards: expand("{dir}/solutions", dir = config['dir'][wildcards.model]),
		ped = lambda wildcards: expand("{dir}/renadd0{animal_effect}.ped", dir = config['dir'][wildcards.model], animal_effect = config['animal_effect'][wildcards.model]),
		script = "source_functions/setup.snp1101.R",
		accuracy_script = "source_functions/calculate_acc.R",
		ped_inb = "data/derived_data/grm_inbreeding/ped_inb.csv"
	params:
	    # R module on lewis
		r_module = config['r_module'],
		# BLUPF90 run directory associated with the current model, pulling from the config file
		dir = lambda wildcards: expand("{dir}", dir = config['dir'][wildcards.model]),
		animal_effect = lambda wildcards: expand("{animal_effect}", animal_effect = config['animal_effect'][wildcards.model]),
		# Genetic variance
		gen_var = lambda wildcards: expand("{gen_var}", gen_var = config['gen_var'][wildcards.model]),
		# heritability
		h2 = lambda wildcards: expand("{gen_var}", gen_var = config['h2'][wildcards.model])
	output:
		traitfile = "data/derived_data/snp1101/{model}/trait.txt"
	shell:
		"""
		module load {params.r_module}
		Rscript --vanilla {input.script} {params.dir} {params.animal_effect} {params.gen_var} {params.h2}
		"""

rule snp1101:
	input:
		traitfile = "data/derived_data/snp1101/{model}/trait.txt",
		ctr = "source_functions/par/snp1101.{model}.ctr",
		# Genotypes formatted as a fixed width file for BLUPF90
		fwf = config['geno_prefix'] + '.fwf.txt',
		# BLUPF90 map file
		map = config['geno_prefix'] + '.chrinfo.txt'
	params:
		snp1101_path = config['snp1101_path'],
		mpi_module = config['mpi_module']
	output:
		bvs_p = "data/derived_data/snp1101/{model}/out/gwas_ssr_{model}_bvs_p.txt",
		bvs = "data/derived_data/snp1101/{model}/out/gwas_ssr_{model}_bvs.txt",
		ctr = "data/derived_data/snp1101/{model}/out/snp1101.{model}.ctr"
	shell:
		"""
		export OMPI_MCA_btl_openib_if_include='mlx5_3:1'
		module load {params.mpi_module}
		{params.snp1101_path} {input.ctr}
		"""
