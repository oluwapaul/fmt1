
#Load module
module load bioinfo
module load Qiime/2-2022.8


qiime tools import \
--type 'SampleData[PairedEndSequencesWithQuality]' \
--input-path manifest.txt \
--input-format PairedEndFastqManifestPhred33V2 \
--output-path ./output/demux-paired-end.qza


qiime demux summarize \
  --i-data ./output/demux-paired-end.qza \
  --o-visualization ./output3/demux-paired-end.qzv

scp poladele@bell.rcac.purdue.edu:/depot/john2185/data/poladele/fmt1/output3/demux-paired-end.qzv .

qiime dada2 denoise-paired \
  --i-demultiplexed-seqs ./output/demux-paired-end.qza \
  --p-trim-left-f 13 \
  --p-trim-left-r 13 \
  --p-trunc-len-f 250 \
  --p-trunc-len-r 222 \
  --o-table ./output/table.qza \
  --o-representative-sequences ./output/rep-seqs.qza \
  --o-denoising-stats ./output/denoising-stats.qza

#Generating the phylogenetic tree
qiime phylogeny align-to-tree-mafft-fasttree \
  --i-sequences ./output/rep-seqs.qza \
  --o-alignment ./output/aligned-rep-seqs.qza \
  --o-masked-alignment ./output/masked-aligned-rep-seqs.qza \
  --o-tree ./output/unrooted-tree.qza \
  --o-rooted-tree ./output/rooted-tree.qza

qiime diversity alpha-rarefaction \
  --i-table ./output/table.qza \
  --i-phylogeny ./output/rooted-tree.qza \
  --p-max-depth 50000 \
  --m-metadata-file metadata.txt \
  --o-visualization ./output/alpha-rarefaction.qzv

qiime feature-table summarize \
  --i-table ./output/table.qza \
  --o-visualization ./output/table.qzv \
  --m-sample-metadata-file metadata.txt

qiime feature-table tabulate-seqs \
  --i-data ./output/rep-seqs.qza \
  --o-visualization ./output/rep-seqs.qzv
 
qiime metadata tabulate \
  --m-input-file ./output/denoising-stats.qza \
  --o-visualization ./output/denoising-stats.qzv


qiime diversity core-metrics-phylogenetic \
  --i-phylogeny ./output/rooted-tree.qza \
  --i-table ./output/table.qza \
  --p-sampling-depth 45110 \
  --m-metadata-file metadata.txt \
  --output-dir ./output/core-metrics-results


# Jaccard significance test
qiime diversity beta-group-significance \
  --i-distance-matrix ./output/core-metrics-results/jaccard_distance_matrix.qza \
  --m-metadata-file metadata.txt \
  --m-metadata-column day \
  --o-visualization ./output/core-metrics-results/jaccard-day-significance.qzv \
  --p-pairwise

qiime diversity beta-group-significance \
  --i-distance-matrix ./output/core-metrics-results/jaccard_distance_matrix.qza \
  --m-metadata-file metadata.txt \
  --m-metadata-column type \
  --o-visualization ./output/core-metrics-results/jaccard-type-significance.qzv \
  --p-pairwise

qiime diversity beta-group-significance \
  --i-distance-matrix ./output/core-metrics-results/jaccard_distance_matrix.qza \
  --m-metadata-file metadata.txt \
  --m-metadata-column group \
  --o-visualization ./output/core-metrics-results/jaccard-group-significance.qzv \
  --p-pairwise

#To filter the fecal sample
#Jaccard
qiime diversity filter-distance-matrix \
  --i-distance-matrix fecal_jaccard-distance-matrix.qza \
  --m-metadata-file metadata_fecal_d0.txt \
  --p-where "[day]='0'" \
  --o-filtered-distance-matrix d0_fecal_jaccard-distance-matrix.qza

qiime diversity filter-distance-matrix \
  --i-distance-matrix fecal_jaccard-distance-matrix.qza \
  --m-metadata-file metadata_fecal_d2.txt \
  --p-where "[day]='2'" \
  --o-filtered-distance-matrix d2_fecal_jaccard-distance-matrix.qza

qiime diversity filter-distance-matrix \
  --i-distance-matrix fecal_jaccard-distance-matrix.qza \
  --m-metadata-file metadata_fecal_d5.txt \
  --p-where "[day]='5'" \
  --o-filtered-distance-matrix d5_fecal_jaccard-distance-matrix.qza

qiime diversity filter-distance-matrix \
  --i-distance-matrix fecal_jaccard-distance-matrix.qza \
  --m-metadata-file metadata_fecal_d7.txt \
  --p-where "[day]='7'" \
  --o-filtered-distance-matrix d7_fecal_jaccard-distance-matrix.qza


#Testing significance by day
#Jaccard
qiime diversity beta-group-significance \
  --i-distance-matrix d0_fecal_jaccard-distance-matrix.qza \
  --m-metadata-file metadata_d0.txt \
  --m-metadata-column trt \
  --o-visualization d0_fecal_jaccard-group-significance.qzv \
  --p-pairwise

qiime diversity beta-group-significance \
  --i-distance-matrix d2_fecal_jaccard-distance-matrix.qza \
  --m-metadata-file metadata_d2.txt \
  --m-metadata-column trt \
  --o-visualization d2_fecal_jaccard-group-significance.qzv \
  --p-pairwise

qiime diversity beta-group-significance \
  --i-distance-matrix d5_fecal_jaccard-distance-matrix.qza \
  --m-metadata-file metadata_d5.txt \
  --m-metadata-column trt \
  --o-visualization d5_fecal_jaccard-group-significance.qzv \
  --p-pairwise

qiime diversity beta-group-significance \
  --i-distance-matrix d7_fecal_jaccard-distance-matrix.qza \
  --m-metadata-file metadata_d7.txt \
  --m-metadata-column trt \
  --o-visualization d7_fecal_jaccard-group-significance.qzv \
  --p-pairwise


qiime diversity beta-group-significance \
  --i-distance-matrix d7_fecal_jaccard-distance-matrix.qza \
  --m-metadata-file metadata_d7.txt \
  --m-metadata-column trt \
  --p-method permdisp \
  --o-visualization disp_d7_fecal_jaccard-group-significance.qzv \
  --p-pairwise

qiime feature-classifier classify-sklearn \
  --i-classifier silva-138-99-515-806-nb-classifier.qza \
  --i-reads ./output/rep-seqs.qza \
  --o-classification ./output/taxonomy.qza

qiime metadata tabulate \
  --m-input-file ./output/taxonomy.qza \
  --o-visualization ./output/taxonomy.qzv

qiime taxa barplot \
  --i-table ./output/table.qza \
  --i-taxonomy ./output/taxonomy.qza \
  --m-metadata-file metadata.txt \
  --o-visualization ./output/taxa-bar-plots.qzv

qiime taxa collapse \
  --i-table ./output/table.qza \
  --i-taxonomy ./output/taxonomy.qza \
  --p-level 6 \
  --o-collapsed-table ./output/table-l6.qza

qiime taxa collapse \
  --i-table ./output/table.qza \
  --i-taxonomy ./output/taxonomy.qza \
  --p-level 5 \
  --o-collapsed-table ./output/table-l5.qza

qiime taxa collapse \
  --i-table ./output/table.qza \
  --i-taxonomy ./output/taxonomy.qza \
  --p-level 6 \
  --o-collapsed-table ./output/table-l6.qza

qiime taxa collapse \
  --i-table rarefied_table.qza \
  --i-taxonomy ../taxonomy.qza \
  --p-level 6\
  --o-collapsed-table rarefied_table-l6.qza

qiime tools export \
  --input-path rarefied_table-l6.qza \
  --output-path exported-feature-rarefied_table-l6

biom convert -i feature-table.biom -o feature-rarefied_table-l6.tsv --to-tsv
