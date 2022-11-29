# snakemake -s source_functions/gcta_gwas.years.snakefile -j 400 --rerun-incomplete --latency-wait 30 --config --cluster-config source_functions/cluster_config/gcta_gwas.years.cluster.json --cluster "sbatch -p {cluster.p} -o {cluster.o} --account {cluster.account} -t {cluster.t} -c {cluster.c} --mem {cluster.mem} --account {cluster.account} --mail-user {cluster.mail-user} --mail-type {cluster.mail-type}" -p &> log/snakemake_log/gcta_gwas.years/201214.gcta_gwas.years.log

import os

configfile: "source_functions/config/gcta_gwas.years.config.yaml"

# Make log directories if they don't exist
os.makedirs("log/slurm_out/gcta_gwas.years", exist_ok = True)
for x in expand("log/slurm_out/gcta_gwas.years/{rules}", rules = config['rules']):
	os.makedirs(x, exist_ok = True)

rule all:
	input: expand("data/derived_data/gcta_gwas.years/{year}/{year}.mlma", year = config['year'])

rule pheno_file:
	input:
		script = "source_functions/setup.gcta_gwas.years.R",
		cleaned = "data/derived_data/import_join_clean/cleaned.rds",
		genotyped = "data/derived_data/grm_inbreeding/mizzou_hairshed.diagonal.full_reg.csv",
		score_groups = "data/derived_data/score_groups.xlsx",
		ua_score_groups = "data/derived_data/ua_score_groups.xlsx",
		full_ped = "data/derived_data/3gen/full_ped.rds"
	params:
		r_module = config['r_module'],
		year = "{year}"
	output:
		pheno = "data/derived_data/gcta_gwas.years/{year}/pheno.txt",
		covar = "data/derived_data/gcta_gwas.years/{year}/covar.txt"
	shell:
		"""
		module load {params.r_module}
		Rscript --vanilla {input.script} {params.year}
		"""

rule mlma:
	input:
		plink = expand("{geno_prefix}.qc.{extension}", geno_prefix = config['geno_prefix'], extension = ['bed', 'bim', 'fam']),
		grm_gz = config['grm_prefix'] + ".grm.gz",
		pheno = "data/derived_data/gcta_gwas.years/{year}/pheno.txt",
		covar = "data/derived_data/gcta_gwas.years/{year}/covar.txt"
	params:
		gcta_path = config['gcta_path'],
		mpi_module = config['mpi_module'],
		in_prefix = config['geno_prefix'] + '.qc',
		threads = config['mlma_threads'],
		grm_prefix = config['grm_prefix'],
		out_prefix = "data/derived_data/gcta_gwas.years/{year}/{year}"
	output:
		out = "data/derived_data/gcta_gwas.years/{year}/{year}.mlma"
	shell:
		"""
		export OMPI_MCA_btl_openib_if_include='mlx5_3:1'
		module load {params.mpi_module}
		{params.gcta_path} --mlma --bfile {params.in_prefix} --pheno {input.pheno} --covar {input.covar} --grm-gz {params.grm_prefix}  --thread-num {params.threads} --out {params.out_prefix}
		"""
