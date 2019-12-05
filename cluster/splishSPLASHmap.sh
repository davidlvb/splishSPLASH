#!/bin/bash
# Accessory script to run on cluster to map FASTQ files to reference genome
# and generate input files for splishSPLASH software

usage="splishSPLASHmap.sh [-h] [-t] -f FILE.FASTQ.GZ -g /PATH/TO/REFGENOME -o /OUTPUT/DIR   
   
where:  
    -h  show this help text 
    -t  number of threads (default: 16)
    -f  input FASTQ.GZ file (usually pre-trimmed) 
    -g  genome directory pre-generated with STAR 
    -o  output directory" 

if [ "$1" == "-h" ] || [ "$1" == "--help" ] ; then
    printf "%s" $usage
    exit 1
fi

# Read param file from command line
while getopts h:t:f:g:o: option
do
	case "${option}"
	in
	h) ;;
	t) THREADS=${OPTARG};;
	f) FASTAFILE=${OPTARG};;
	g) GENOMEFOLDER=${OPTARG};;
	o) OUTDIR=${OPTARG};;
	esac
done

die() { echo "$*" 1>&2 ; exit 1; }
# Check input options have been set
# THREADS=( $(grep -c ^processor /proc/cpuinfo) )
[ "$THREADS" ] || THREADS=16
[ "$FASTAFILE" ] || die  "-f input FASTA file required"
[ "$GENOMEFOLDER" ] || die "-g STAR reference genome directory required"
[ "$OUTDIR" ] || die "-o output directory required"

echo ""
echo "   .---.,---. ,-.   ,-.  .---..-. .-.  .---.,---. ,-.     .--.    .---..-. .-. ";
echo "  ( .-._| .-.\| |   |(| ( .-._| | | | ( .-._| .-.\| |    / /\ \  ( .-._| | | | ";
echo " (_) \  | |-' | |   (_)(_) \  | \`-' |(_) \  | |-' | |   / /__\ \(_) \  | \`-' | ";
echo " _  \ \ | |--'| |   | |_  \ \ | .-. |_  \ \ | |--'| |   |  __  |_  \ \ | .-. | ";
echo "( \`-'  )| |   | \`--.| ( \`-'  )| | |)( \`-'  )| |   | \`--.| |  |)( \`-'  )| | |)| ";
echo " \`----' /(    |( __.\`-'\`----' /(  (_)\`----' /(    |( __.|_|  (_)\`----' /(  (_) ";
echo "       (__)   (_)            (__)          (__)   (_)                 (__) 2.0 ";
echo ""
echo ""
echo "Running with the following options:"
echo "   number of threads: $THREADS"
echo "   input file: $FASTAFILE"
echo "   genome folder: $GENOMEFOLDER"
echo "   output folder: $OUTDIR"
echo ""

# Define mapping functions
function mapSameSegChim {
STAR \
--runThreadN $5   \
--genomeDir  $3     \
--outFileNamePrefix  $2/    \
--readFilesIn   $1 \
--readFilesCommand zcat   \
--alignSoftClipAtReferenceEnds  Yes      \
--alignIntronMin $4 \
--alignSJoverhangMin 20 \
--outFilterMatchNmin 40 \
--outSJfilterOverhangMin 20 20 20 20    \
--outSJfilterCountUniqueMin 1 1 1 1   \
--outSJfilterCountTotalMin 1 1 1 1   \
--outSJfilterDistToOtherSJmin 0 0 0 0    \
--outSJfilterReads All      \
--outSAMtype BAM Unsorted  \
--scoreGapNoncan   0       \
--scoreGapGCAG  0    \
--scoreGapATAC   0      \
--scoreGenomicLengthLog2scale  0    \
--outSAMmultNmax 1  \
--outFilterMismatchNoverLmax 0.1
}

# Define mapping functions
function mapChim {
STAR \
--chimSegmentMin 20  \
--chimJunctionOverhangMin 20 \
--chimScoreJunctionNonGTAG 0 \
--runThreadN $4   \
--outFilterMismatchNoverLmax 0.1   \
--genomeDir $3 \
--readFilesIn $1     \
--readFilesCommand zcat   \
--outSAMtype BAM Unsorted \
--outFileNamePrefix $2/
}

# setup output directories
mkdir -p $OUTDIR
mkdir -p $OUTDIR/mapChim
mkdir -p $OUTDIR/mapSame



echo "Mapping chimeric reads.........."
echo ".......... start time: $(date)"
mapChim $FASTAFILE $OUTDIR/mapChim $GENOMEFOLDER $THREADS
cp $GENOMEFOLDER/*.fa* $OUTDIR/
rm -f $OUTDIR/*.fai
gzip --keep $OUTDIR/mapChim/Chimeric.out.junction
mv $OUTDIR/mapChim/Chimeric.out.junction.gz $OUTDIR/Chimeric.out.junction.txt.gz 
echo ".......... Done! end time: $(date)"
echo ""
echo "Mapping same segment chimeric reads.........."
echo ".......... start time: $(date)"
mapSameSegChim  $FASTAFILE $OUTDIR/mapSame $GENOMEFOLDER 30 $THREADS
echo ".......... extracting 'spliced' reads"
samtools view -h $OUTDIR/mapSame/Aligned.out.bam | awk '$1 ~ /@/ || ( $6 ~ /N/ )'  | samtools view -bS - | samtools sort > $OUTDIR/mapSame/Aligned.out.spliced.bam
samtools index $OUTDIR/mapSame/Aligned.out.spliced.bam
samtools view -h -F 16 $OUTDIR/mapSame/Aligned.out.spliced.bam | awk -v OFS='\t' '{ print $1,$3,"+",$4,$6}' > $OUTDIR/CIGAR.same.out.txt
samtools view -h -f 16  $OUTDIR/mapSame/Aligned.out.spliced.bam | awk -v OFS='\t' '{ print $1,$3,"-",$4,$6}' >> $OUTDIR/CIGAR.same.out.txt
gzip $OUTDIR/CIGAR.same.out.txt
echo ".......... Done! end time: $(date)"
echo ""
echo "~splishSPLASH v2.0 pipeline finished~"
echo ""
