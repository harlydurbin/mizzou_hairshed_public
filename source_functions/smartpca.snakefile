# snakemake -s source_functions/smartpca.snakefile -j 1000 --rerun-incomplete --keep-going --latency-wait 30 --resources load=100 --config --cluster-config source_functions/cluster_config/smartpca.cluster.json --cluster "sbatch -p {cluster.p} -o {cluster.o} --account {cluster.account} -t {cluster.t} -c {cluster.c} --mem {cluster.mem} --account {cluster.account} --mail-user {cluster.mail-user} --mail-type {cluster.mail-type} --qos {cluster.qos}" -p &> log/snakemake_log/smartpca/201030.smartpca.log

import os
import re

configfile: "source_functions/config/smartpca.config.yaml"

#Make log directories if they don't exist
os.makedirs("log/slurm_out/smartpca", exist_ok = True)
for x in expand("log/slurm_out/smartpca/{rules}", rules = config['rules']):
    os.makedirs(x, exist_ok = True)

rule smartpca_all:
	input:
		"data/derived_data/smartpca/smartpca.mizzou_hairshed.evec"

rule pop_labels:
	input:
		fam = config['geno_prefix'] + '.qc.fam',
		script = "source_functions/pedind_poplabels.R"
	params:
		r_module = config['r_module']
	output:
		pedind = "data/derived_data/smartpca/smartpca.mizzou_hairshed.pedind"
	shell:
		"""
		module load {params.r_module}
		Rscript --vanilla {input.script} {input.fam} {output.pedind}
		"""

rule recode_input:
	input:
	# If projecting aurochs, need to use the use the same PLINK files but dataset name won't match, pull 'base_dataset' from dictionary in config
		bed = config['geno_prefix'] + '.qc.bed',
		bim = config['geno_prefix'] + '.qc.bim'
	output:
		bed = "data/derived_data/smartpca/smartpca.mizzou_hairshed.bed",
		pedsnp = "data/derived_data/smartpca/smartpca.mizzou_hairshed.pedsnp"
	shell:
		"""
		cp {input.bed} {output.bed}
		cp {input.bim} {output.pedsnp}
		"""

rule create_par:
	input:
		bed = "data/derived_data/smartpca/smartpca.mizzou_hairshed.bed",
		pedsnp = "data/derived_data/smartpca/smartpca.mizzou_hairshed.pedsnp",
		pedind = "data/derived_data/smartpca/smartpca.mizzou_hairshed.pedind"
	params:
		evec = "data/derived_data/smartpca/smartpca.mizzou_hairshed.evec",
		eval = "data/derived_data/smartpca/smartpca.mizzou_hairshed.eval"
	output:
		par = "data/derived_data/smartpca/smartpca.mizzou_hairshed.par"
	run:
		shell('echo -e "genotypename:\\t{input.bed}\\nsnpname:\\t{input.pedsnp}\\nindivname:\\t{input.pedind}\\nevecoutname:\\t{params.evec}\\nevaloutname:\\t{params.eval}\\nnumchrom:\\t29\\nnumoutlieriter:\\t0\\nfastmode:\\tYES" > {output.par}')

rule smartpca:
	input:
		bed = "data/derived_data/smartpca/smartpca.mizzou_hairshed.bed",
		pedsnp = "data/derived_data/smartpca/smartpca.mizzou_hairshed.pedsnp",
		pedind = "data/derived_data/smartpca/smartpca.mizzou_hairshed.pedind",
		par = "data/derived_data/smartpca/smartpca.mizzou_hairshed.par"
	params:
		eigensoft_module = config['eigensoft_module'],
	output:
		evec = "data/derived_data/smartpca/smartpca.mizzou_hairshed.evec"
	shell:
		"""
		module load {params.eigensoft_module}
		smartpca -p {input.par}
		"""
