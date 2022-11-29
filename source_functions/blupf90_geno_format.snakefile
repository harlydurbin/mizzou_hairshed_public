# snakemake -s source_functions/blupf90_geno_format.snakefile -j 400 --rerun-incomplete --latency-wait 30 --config --cluster-config source_functions/cluster_config/blupf90_geno_format.cluster.json --cluster "sbatch -p {cluster.p} -o {cluster.o} --account {cluster.account} -t {cluster.t} -c {cluster.c} --mem {cluster.mem} --account {cluster.account} --mail-user {cluster.mail-user} --mail-type {cluster.mail-type}" -p &> log/snakemake_log/blupf90_geno_format/201002.blupf90_geno_format.log

import os

configfile: "source_functions/config/blupf90_geno_format.config.yaml"

# Make log directories if they don't exist
os.makedirs("log/slurm_out/blupf90_geno_format", exist_ok = True)
for x in expand("log/slurm_out/blupf90_geno_format/{rules}", rules = config['rules']):
	os.makedirs(x, exist_ok = True)

rule format_all:
	input: config['geno_prefix'] + '.fwf.txt', config['geno_prefix'] + '.chrinfo.txt', config['geno_prefix'] + '.chrpos.txt'

rule remove_list_key:
	input:
		fam = config['geno_prefix'] + '.fam',
		remove_list = "data/derived_data/seekparentf90/parentage_conflicts.removelist",
		script = "source_functions/remove_list_key.R"
	params:
		r_module = config['r_module'],
		geno_prefix = config['geno_prefix']
	output:
		update_id = config['geno_prefix'] + '.update_id.txt',
		remove_list = config['geno_prefix'] + '.remove.txt'
	shell:
		"""
		module load {params.r_module}
		Rscript --vanilla {input.script} {params.geno_prefix}
		"""

rule qc:
	input:
		plink = expand("{prefix}.{extension}", prefix = config['geno_prefix'], extension = ['bed', 'bim', 'fam']),
		update_id = config['geno_prefix'] + '.update_id.txt',
		remove_list = config['geno_prefix'] + '.remove.txt'
	params:
		plink_module = config['plink_module'],
		nt = config['plink_nt'],
		prefix_in = config['geno_prefix'],
		prefix_out = config['geno_prefix'] + '.qc',
		maf = config['maf']
	output:
		plink = expand("{prefix}.qc.{extension}", prefix = config['geno_prefix'], extension = ['bed', 'bim', 'fam'])
	# Remove flagged samples
	# Filter on MAF
	# Update IDs
	shell:
		"""
		module load {params.plink_module}
		plink --bfile {params.prefix_in} --double-id --cow --threads {params.nt} --remove {input.remove_list} --maf {params.maf} --update-ids {input.update_id} --make-bed --out {params.prefix_out}
		"""

# Some blupf90 programs want a map file as chr:pos
# Some blupf90 programs want a map file as "snp_name chr pos"
# Creates both
rule mapfile:
	input:
		bim = config['geno_prefix'] + '.qc.bim',
		script = "source_functions/blupf90_map.R"
	params:
		r_module = config['r_module'],
		geno_prefix = config['geno_prefix']
	output:
		chrinfo = config['geno_prefix'] + '.chrinfo.txt',
		chr_pos = config['geno_prefix'] + '.chrpos.txt'
	shell:
		"""
		module load {params.r_module}
		Rscript --vanilla {input.script} {input.bim} {params.geno_prefix}
		"""

# Convert PLINK bed/bim/bam to PLINK .raw additive file
rule recode_a:
	input:
		plink = expand("{prefix}.qc.{extension}", prefix = config['geno_prefix'], extension = ['bed', 'bim', 'fam'])
	params:
		plink_module = config['plink_module'],
		nt = config['plink_nt'],
		prefix_in = config['geno_prefix'] + '.qc',
		prefix_out = config['geno_prefix']
	output:
		recoded = config['geno_prefix'] + '.raw'
	shell:
		"""
		module load {params.plink_module}
		plink --bfile {params.prefix_in} --double-id --cow --threads {params.nt} --recode A --out {params.prefix_out}
		"""

rule remove_spaces:
	input:
		recoded = config['geno_prefix'] + ".raw"
	output:
		temp = temp(config['geno_prefix'] + '.temp1.txt'),
		full_reg = config['geno_prefix'] + '.full_reg.txt'
	# cut -d " " -f 7- removed first 6 columns
	# 1d removes header line
	# s/ //g removes spaces such that each row is an 850K long string
	shell:
		"""
		awk '{{print $1}}' {input.recoded} | sed '1d' > {output.full_reg}
		cut -d " " -f 7- {input.recoded} | sed '1d; s/ //g' > {output.temp}
		"""

# Append column of IDs
# I'm lazy and cant figure out how to pipe to awk -v
rule append_id:
	input:
		temp = config['geno_prefix'] + '.temp1.txt',
		full_reg = config['geno_prefix'] + ".full_reg.txt"
	output:
		formatted = temp(config['geno_prefix'] + '.temp2.txt')
	# paste IDs
	shell:
		"""
		awk -v f2={input.temp} ' {{ c = $1; getline < f2; print c, $1; }} ' {input.full_reg} > {output.formatted}
		"""

rule fwf:
	input:
		formatted = config['geno_prefix'] + '.temp2.txt'
	output:
		fwf = config['geno_prefix'] + '.fwf.txt'
	# awk command creates fixed width file
	# grep command removes genotypes for ids in conflict file
	shell:
		"""
		awk '{{printf "%-20s %s\\n", $1, $2}}' {input.formatted} &> {output.fwf}
		"""
