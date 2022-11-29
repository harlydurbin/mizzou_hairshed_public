# snakemake -s source_functions/seekparentf90.snakefile -j 400 --rerun-incomplete --latency-wait 30 --config --cluster-config source_functions/cluster_config/seekparentf90.cluster.json --cluster "sbatch -p {cluster.p} -o {cluster.o} --account {cluster.account} -t {cluster.t} -c {cluster.c} --mem {cluster.mem} --account {cluster.account} --mail-user {cluster.mail-user} --mail-type {cluster.mail-type}" -p &> log/snakemake_log/seekparentf90/201002.seekparentf90.log

import os

configfile: "source_functions/config/seekparentf90.config.yaml"

# Make log directories if they don't exist
os.makedirs("log/slurm_out/seekparentf90", exist_ok = True)
for x in expand("log/slurm_out/seekparentf90/{rules}", rules = config['rules']):
	os.makedirs(x, exist_ok = True)

rule seekparent_all:
	input: "data/derived_data/seekparentf90/Parent_Progeny_Conflicts.condensed.txt", "data/derived_data/seekparentf90/Parent_Progeny_Conflicts_Trio.condensed.txt"

rule setup:
	input:
		fwf = config['geno_prefix'] + '.fwf.txt',
		blupf90_ped = "data/derived_data/3gen/blupf90_ped.txt",
		chr_info = config['geno_prefix'] + '.chrinfo.txt'
	output:
		genotypes = temp("data/derived_data/seekparentf90/genotypes.txt"),
		ped = temp("data/derived_data/seekparentf90/ped.txt"),
		map = temp("data/derived_data/seekparentf90/map.txt")
	shell:
		# echo command adds header seekparentf90 needs to map file
		"""
		cp {input.fwf} {output.genotypes}
		cp {input.blupf90_ped} {output.ped}
		echo -e "SNP_ID CHR POS" | cat - {input.chr_info} > {output.map}
		"""

rule seekparentf90:
	input:
		genotypes = "data/derived_data/seekparentf90/genotypes.txt",
		ped = "data/derived_data/seekparentf90/ped.txt",
		map = "data/derived_data/seekparentf90/map.txt"
	params:
		path = config['seekparentf90_path'],
		directory = "data/derived_data/seekparentf90",
		seektype = config['seektype'],
		psrecord = "/storage/hpc/group/UMAG/WORKING/hjdzpd/mizzou_hairshed/log/psrecord/seekparentf90/seekparentf90/seekparentf90.log"
	output:
		"data/derived_data/seekparentf90/Parent_Progeny_Conflicts.txt",
		"data/derived_data/seekparentf90/Parent_Progeny_Conflicts_Trio.txt"
	shell:
		"""
		cd {params.directory}
		psrecord "{params.path} --pedfile ped.txt --snpfile genotypes.txt --mapfile map.txt --seektype {params.seektype} --trio --maxsnp 1000000 --full_log_checks --chr_x 30" --log {params.psrecord} --include-children --interval 2
		"""

rule condense:
	input:
		conflicts = "data/derived_data/seekparentf90/Parent_Progeny_Conflicts.txt",
		conflicts_trio = "data/derived_data/seekparentf90/Parent_Progeny_Conflicts_Trio.txt"
	output:
		condensed = "data/derived_data/seekparentf90/Parent_Progeny_Conflicts.condensed.txt",
		condensed_trio = "data/derived_data/seekparentf90/Parent_Progeny_Conflicts_Trio.condensed.txt"
	shell:
		"""
		sed '1d' {input.conflicts} | grep -a "^Animal" > {output.condensed}
		"""
