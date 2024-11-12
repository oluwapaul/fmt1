{\rtf1\ansi\ansicpg1252\cocoartf2709
\cocoatextscaling0\cocoaplatform0{\fonttbl\f0\fnil\fcharset0 .AppleSystemUIFontMonospaced-Regular;\f1\fnil\fcharset0 Menlo-Regular;\f2\fmodern\fcharset0 Courier;
}
{\colortbl;\red255\green255\blue255;\red27\green31\blue35;\red244\green246\blue249;\red0\green0\blue0;
\red38\green38\blue38;\red242\green242\blue242;\red83\green83\blue83;\red29\green111\blue63;}
{\*\expandedcolortbl;;\cssrgb\c14118\c16078\c18431;\cssrgb\c96471\c97255\c98039;\csgray\c0;
\cssrgb\c20000\c20000\c20000;\cssrgb\c96078\c96078\c96078;\cssrgb\c40000\c40000\c40000;\cssrgb\c12549\c50196\c31373;}
\margl1440\margr1440\vieww14700\viewh8400\viewkind0
\deftab720
\pard\pardeftab720\partightenfactor0

\f0\fs27\fsmilli13600 \cf2 \cb3 \expnd0\expndtw0\kerning0
\
\pard\tx560\tx1120\tx1680\tx2240\tx2800\tx3360\tx3920\tx4480\tx5040\tx5600\tx6160\tx6720\pardeftab720\pardirnatural\partightenfactor0

\f1\fs22 \cf4 \cb1 \kerning1\expnd0\expndtw0 \CocoaLigature0 #Load module\
module load bioinfo\
module load Qiime/2-2022.8\
\
\pard\pardeftab720\partightenfactor0

\f0\fs27\fsmilli13600 \cf2 \cb3 \expnd0\expndtw0\kerning0
\CocoaLigature1 \
qiime tools import \\\
--type 'SampleData[PairedEndSequencesWithQuality]' \\\
--input-path manifest.txt \\\
--input-format PairedEndFastqManifestPhred33V2 \\\
--output-path ./output/demux-paired-end.qza\
\
\
qiime demux summarize \\\
  --i-data ./output/demux-paired-end.qza \\\
  --o-visualization ./output3/demux-paired-end.qzv\
\
\pard\tx560\tx1120\tx1680\tx2240\tx2800\tx3360\tx3920\tx4480\tx5040\tx5600\tx6160\tx6720\pardeftab720\pardirnatural\partightenfactor0

\f1\fs22 \cf4 \cb1 \kerning1\expnd0\expndtw0 \CocoaLigature0 scp poladele@bell.rcac.purdue.edu:/depot/john2185/data/poladele/fmt1/output3/demux-paired-end.qzv .\
\
\pard\pardeftab720\partightenfactor0

\f0\fs27\fsmilli13600 \cf2 \cb3 \expnd0\expndtw0\kerning0
\CocoaLigature1 qiime dada2 denoise-paired \\\
  --i-demultiplexed-seqs ./output/demux-paired-end.qza \\\
  --p-trim-left-f 13 \\\
  --p-trim-left-r 13 \\\
  --p-trunc-len-f 250 \\\
  --p-trunc-len-r 222 \\\
  --o-table ./output/table.qza \\\
  --o-representative-sequences ./output/rep-seqs.qza \\\
  --o-denoising-stats ./output/denoising-stats.qza\
\
#Generating the phylogenetic tree\
qiime phylogeny align-to-tree-mafft-fasttree \\\
  --i-sequences ./output/rep-seqs.qza \\\
  --o-alignment ./output/aligned-rep-seqs.qza \\\
  --o-masked-alignment ./output/masked-aligned-rep-seqs.qza \\\
  --o-tree ./output/unrooted-tree.qza \\\
  --o-rooted-tree ./output/rooted-tree.qza\
\
\pard\tx560\tx1120\tx1680\tx2240\tx2800\tx3360\tx3920\tx4480\tx5040\tx5600\tx6160\tx6720\pardeftab720\pardirnatural\partightenfactor0

\f1\fs22 \cf4 \cb1 \kerning1\expnd0\expndtw0 \CocoaLigature0 qiime diversity alpha-rarefaction \\\
  --i-table ./output/table.qza \\\
  --i-phylogeny ./output/rooted-tree.qza \\\
  --p-max-depth 50000 \\\
  --m-metadata-file metadata.txt \\\
  --o-visualization ./output/alpha-rarefaction.qzv
\f0\fs27\fsmilli13600 \cf2 \cb3 \expnd0\expndtw0\kerning0
\CocoaLigature1 \
\pard\pardeftab720\partightenfactor0
\cf2 \
qiime feature-table summarize \\\
  --i-table ./output/table.qza \\\
  --o-visualization ./output/table.qzv \\\
  --m-sample-metadata-file metadata.txt\
\
qiime feature-table tabulate-seqs \\\
  --i-data ./output/rep-seqs.qza \\\
  --o-visualization ./output/rep-seqs.qzv\
 \
qiime metadata tabulate \\\
  --m-input-file ./output/denoising-stats.qza \\\
  --o-visualization ./output/denoising-stats.qzv\
\
\
\pard\tx560\tx1120\tx1680\tx2240\tx2800\tx3360\tx3920\tx4480\tx5040\tx5600\tx6160\tx6720\pardeftab720\pardirnatural\partightenfactor0

\f1\fs22 \cf4 \cb1 \kerning1\expnd0\expndtw0 \CocoaLigature0 qiime diversity core-metrics-phylogenetic \\\
  --i-phylogeny ./output/rooted-tree.qza \\\
  --i-table ./output/table.qza \\\
  --p-sampling-depth 45110 \\\
  --m-metadata-file metadata.txt \\\
  --output-dir ./output/core-metrics-results\
\
\
# Jaccard significance test\
qiime diversity beta-group-significance \\\
  --i-distance-matrix ./output/core-metrics-results/jaccard_distance_matrix.qza \\\
  --m-metadata-file metadata.txt \\\
  --m-metadata-column day \\\
  --o-visualization ./output/core-metrics-results/jaccard-day-significance.qzv \\\
  --p-pairwise\
\
qiime diversity beta-group-significance \\\
  --i-distance-matrix ./output/core-metrics-results/jaccard_distance_matrix.qza \\\
  --m-metadata-file metadata.txt \\\
  --m-metadata-column type \\\
  --o-visualization ./output/core-metrics-results/jaccard-type-significance.qzv \\\
  --p-pairwise\
\pard\pardeftab720\partightenfactor0

\f0\fs27\fsmilli13600 \cf2 \cb3 \expnd0\expndtw0\kerning0
\CocoaLigature1 \
\pard\tx560\tx1120\tx1680\tx2240\tx2800\tx3360\tx3920\tx4480\tx5040\tx5600\tx6160\tx6720\pardeftab720\pardirnatural\partightenfactor0

\f1\fs22 \cf4 \cb1 \kerning1\expnd0\expndtw0 \CocoaLigature0 qiime diversity beta-group-significance \\\
  --i-distance-matrix ./output/core-metrics-results/jaccard_distance_matrix.qza \\\
  --m-metadata-file metadata.txt \\\
  --m-metadata-column group \\\
  --o-visualization ./output/core-metrics-results/jaccard-group-significance.qzv \\\
  --p-pairwise
\f0\fs27\fsmilli13600 \cf2 \cb3 \expnd0\expndtw0\kerning0
\CocoaLigature1 \
\pard\pardeftab720\partightenfactor0
\cf2 \
#To filter the fecal sample\
#Jaccard\
qiime diversity filter-distance-matrix \\\
  --i-distance-matrix fecal_
\f1\fs22 \cf4 \cb1 \kerning1\expnd0\expndtw0 \CocoaLigature0 jaccard-
\f0\fs27\fsmilli13600 \cf2 \cb3 \expnd0\expndtw0\kerning0
\CocoaLigature1 distance-matrix.qza \\\
  --m-metadata-file metadata_fecal_d0.txt \\\
  --p-where "[day]='0'" \\\
  --o-filtered-distance-matrix d0_fecal_
\f1\fs22 \cf4 \cb1 \kerning1\expnd0\expndtw0 \CocoaLigature0 jaccard
\f0\fs27\fsmilli13600 \cf2 \cb3 \expnd0\expndtw0\kerning0
\CocoaLigature1 -distance-matrix.qza\
\
qiime diversity filter-distance-matrix \\\
  --i-distance-matrix fecal_
\f1\fs22 \cf4 \cb1 \kerning1\expnd0\expndtw0 \CocoaLigature0 jaccard-
\f0\fs27\fsmilli13600 \cf2 \cb3 \expnd0\expndtw0\kerning0
\CocoaLigature1 distance-matrix.qza \\\
  --m-metadata-file metadata_fecal_d2.txt \\\
  --p-where "[day]='2'" \\\
  --o-filtered-distance-matrix d2_fecal_
\f1\fs22 \cf4 \cb1 \kerning1\expnd0\expndtw0 \CocoaLigature0 jaccard
\f0\fs27\fsmilli13600 \cf2 \cb3 \expnd0\expndtw0\kerning0
\CocoaLigature1 -distance-matrix.qza\
\
qiime diversity filter-distance-matrix \\\
  --i-distance-matrix fecal_
\f1\fs22 \cf4 \cb1 \kerning1\expnd0\expndtw0 \CocoaLigature0 jaccard-
\f0\fs27\fsmilli13600 \cf2 \cb3 \expnd0\expndtw0\kerning0
\CocoaLigature1 distance-matrix.qza \\\
  --m-metadata-file metadata_fecal_d5.txt \\\
  --p-where "[day]='5'" \\\
  --o-filtered-distance-matrix d5_fecal_
\f1\fs22 \cf4 \cb1 \kerning1\expnd0\expndtw0 \CocoaLigature0 jaccard
\f0\fs27\fsmilli13600 \cf2 \cb3 \expnd0\expndtw0\kerning0
\CocoaLigature1 -distance-matrix.qza\
\
qiime diversity filter-distance-matrix \\\
  --i-distance-matrix fecal_
\f1\fs22 \cf4 \cb1 \kerning1\expnd0\expndtw0 \CocoaLigature0 jaccard-
\f0\fs27\fsmilli13600 \cf2 \cb3 \expnd0\expndtw0\kerning0
\CocoaLigature1 distance-matrix.qza \\\
  --m-metadata-file metadata_fecal_d7.txt \\\
  --p-where "[day]='7'" \\\
  --o-filtered-distance-matrix d7_fecal_
\f1\fs22 \cf4 \cb1 \kerning1\expnd0\expndtw0 \CocoaLigature0 jaccard
\f0\fs27\fsmilli13600 \cf2 \cb3 \expnd0\expndtw0\kerning0
\CocoaLigature1 -distance-matrix.qza\
\
\
#Testing significance by day\
#Jaccard\
\pard\tx560\tx1120\tx1680\tx2240\tx2800\tx3360\tx3920\tx4480\tx5040\tx5600\tx6160\tx6720\pardeftab720\pardirnatural\partightenfactor0

\f1\fs22 \cf4 \cb1 \kerning1\expnd0\expndtw0 \CocoaLigature0 qiime diversity beta-group-significance \\\
  --i-distance-matrix 
\f0\fs27\fsmilli13600 \cf2 \cb3 \expnd0\expndtw0\kerning0
\CocoaLigature1 d0_fecal_
\f1\fs22 \cf4 \cb1 \kerning1\expnd0\expndtw0 \CocoaLigature0 jaccard-distance-matrix.qza \\\
  --m-metadata-file metadata_d0.txt \\\
  --m-metadata-column trt \\\
  --o-visualization 
\f0\fs27\fsmilli13600 \cf2 \cb3 \expnd0\expndtw0\kerning0
\CocoaLigature1 d0_fecal_
\f1\fs22 \cf4 \cb1 \kerning1\expnd0\expndtw0 \CocoaLigature0 jaccard-group-significance.qzv \\\
  --p-pairwise
\f0\fs27\fsmilli13600 \cf2 \cb3 \expnd0\expndtw0\kerning0
\CocoaLigature1 \
\pard\pardeftab720\partightenfactor0
\cf2 \
\pard\tx560\tx1120\tx1680\tx2240\tx2800\tx3360\tx3920\tx4480\tx5040\tx5600\tx6160\tx6720\pardeftab720\pardirnatural\partightenfactor0

\f1\fs22 \cf4 \cb1 \kerning1\expnd0\expndtw0 \CocoaLigature0 qiime diversity beta-group-significance \\\
  --i-distance-matrix 
\f0\fs27\fsmilli13600 \cf2 \cb3 \expnd0\expndtw0\kerning0
\CocoaLigature1 d2_fecal_
\f1\fs22 \cf4 \cb1 \kerning1\expnd0\expndtw0 \CocoaLigature0 jaccard-distance-matrix.qza \\\
  --m-metadata-file metadata_d2.txt \\\
  --m-metadata-column trt \\\
  --o-visualization 
\f0\fs27\fsmilli13600 \cf2 \cb3 \expnd0\expndtw0\kerning0
\CocoaLigature1 d2_fecal_
\f1\fs22 \cf4 \cb1 \kerning1\expnd0\expndtw0 \CocoaLigature0 jaccard-group-significance.qzv \\\
  --p-pairwise
\f0\fs27\fsmilli13600 \cf2 \cb3 \expnd0\expndtw0\kerning0
\CocoaLigature1 \
\pard\pardeftab720\partightenfactor0
\cf2 \
\pard\tx560\tx1120\tx1680\tx2240\tx2800\tx3360\tx3920\tx4480\tx5040\tx5600\tx6160\tx6720\pardeftab720\pardirnatural\partightenfactor0

\f1\fs22 \cf4 \cb1 \kerning1\expnd0\expndtw0 \CocoaLigature0 qiime diversity beta-group-significance \\\
  --i-distance-matrix 
\f0\fs27\fsmilli13600 \cf2 \cb3 \expnd0\expndtw0\kerning0
\CocoaLigature1 d5_fecal_
\f1\fs22 \cf4 \cb1 \kerning1\expnd0\expndtw0 \CocoaLigature0 jaccard-distance-matrix.qza \\\
  --m-metadata-file metadata_d5.txt \\\
  --m-metadata-column trt \\\
  --o-visualization 
\f0\fs27\fsmilli13600 \cf2 \cb3 \expnd0\expndtw0\kerning0
\CocoaLigature1 d5_fecal_
\f1\fs22 \cf4 \cb1 \kerning1\expnd0\expndtw0 \CocoaLigature0 jaccard-group-significance.qzv \\\
  --p-pairwise
\f0\fs27\fsmilli13600 \cf2 \cb3 \expnd0\expndtw0\kerning0
\CocoaLigature1 \
\pard\pardeftab720\partightenfactor0
\cf2 \
\pard\tx560\tx1120\tx1680\tx2240\tx2800\tx3360\tx3920\tx4480\tx5040\tx5600\tx6160\tx6720\pardeftab720\pardirnatural\partightenfactor0

\f1\fs22 \cf4 \cb1 \kerning1\expnd0\expndtw0 \CocoaLigature0 qiime diversity beta-group-significance \\\
  --i-distance-matrix 
\f0\fs27\fsmilli13600 \cf2 \cb3 \expnd0\expndtw0\kerning0
\CocoaLigature1 d7_fecal_
\f1\fs22 \cf4 \cb1 \kerning1\expnd0\expndtw0 \CocoaLigature0 jaccard-distance-matrix.qza \\\
  --m-metadata-file metadata_d7.txt \\\
  --m-metadata-column trt \\\
  --o-visualization 
\f0\fs27\fsmilli13600 \cf2 \cb3 \expnd0\expndtw0\kerning0
\CocoaLigature1 d7_fecal_
\f1\fs22 \cf4 \cb1 \kerning1\expnd0\expndtw0 \CocoaLigature0 jaccard-group-significance.qzv \\\
  --p-pairwise
\f0\fs27\fsmilli13600 \cf2 \cb3 \expnd0\expndtw0\kerning0
\CocoaLigature1 \
\pard\pardeftab720\partightenfactor0
\cf2 \
\
\pard\tx560\tx1120\tx1680\tx2240\tx2800\tx3360\tx3920\tx4480\tx5040\tx5600\tx6160\tx6720\pardeftab720\pardirnatural\partightenfactor0

\f1\fs22 \cf4 \cb1 \kerning1\expnd0\expndtw0 \CocoaLigature0 qiime diversity beta-group-significance \\\
  --i-distance-matrix 
\f0\fs27\fsmilli13600 \cf2 \cb3 \expnd0\expndtw0\kerning0
\CocoaLigature1 d7_fecal_
\f1\fs22 \cf4 \cb1 \kerning1\expnd0\expndtw0 \CocoaLigature0 jaccard-distance-matrix.qza \\\
  --m-metadata-file metadata_d7.txt \\\
  --m-metadata-column trt \\\
  --p-method permdisp \\\
  --o-visualization disp_
\f0\fs27\fsmilli13600 \cf2 \cb3 \expnd0\expndtw0\kerning0
\CocoaLigature1 d7_fecal_
\f1\fs22 \cf4 \cb1 \kerning1\expnd0\expndtw0 \CocoaLigature0 jaccard-group-significance.qzv \\\
  --p-pairwise
\f0\fs27\fsmilli13600 \cf2 \cb3 \expnd0\expndtw0\kerning0
\CocoaLigature1 \
\pard\pardeftab720\partightenfactor0
\cf2 \
\pard\tx560\tx1120\tx1680\tx2240\tx2800\tx3360\tx3920\tx4480\tx5040\tx5600\tx6160\tx6720\pardeftab720\pardirnatural\partightenfactor0

\f1\fs22 \cf4 \cb1 \kerning1\expnd0\expndtw0 \CocoaLigature0 qiime feature-classifier classify-sklearn \\\
  --i-classifier silva-138-99-515-806-nb-classifier.qza \\\
  --i-reads ./output/rep-seqs.qza \\\
  --o-classification ./output/taxonomy.qza\
\
qiime metadata tabulate \\\
  --m-input-file ./output/taxonomy.qza \\\
  --o-visualization ./output/taxonomy.qzv\
\
qiime taxa barplot \\\
  --i-table ./output/table.qza \\\
  --i-taxonomy ./output/taxonomy.qza \\\
  --m-metadata-file metadata.txt \\\
  --o-visualization ./output/taxa-bar-plots.qzv\
\
qiime taxa collapse \\\
  --i-table ./output/table.qza \\\
  --i-taxonomy ./output/taxonomy.qza \\\
  --p-level 6 \\\
  --o-collapsed-table ./output/table-l6.qza\
\
qiime taxa collapse \\\
  --i-table ./output/table.qza \\\
  --i-taxonomy ./output/taxonomy.qza \\\
  --p-level 5 \\\
  --o-collapsed-table ./output/table-l5.qza
\f0\fs27\fsmilli13600 \cf2 \cb3 \expnd0\expndtw0\kerning0
\CocoaLigature1 \
\pard\pardeftab720\partightenfactor0
\cf2 \
\pard\tx560\tx1120\tx1680\tx2240\tx2800\tx3360\tx3920\tx4480\tx5040\tx5600\tx6160\tx6720\pardeftab720\pardirnatural\partightenfactor0

\f1\fs22 \cf4 \cb1 \kerning1\expnd0\expndtw0 \CocoaLigature0 qiime taxa collapse \\\
  --i-table ./output/table.qza \\\
  --i-taxonomy ./output/taxonomy.qza \\\
  --p-level 6 \\\
  --o-collapsed-table ./output/table-l6.qza
\f0\fs27\fsmilli13600 \cf2 \cb3 \expnd0\expndtw0\kerning0
\CocoaLigature1 \
\
\pard\pardeftab720\partightenfactor0

\f2\fs28 \cf5 \cb6 qiime taxa collapse \\\
  \cf7 --\cf5 i\cf7 -\cf5 table rarefied_table\cf7 .\cf5 qza \\\
  \cf7 --\cf5 i\cf7 -\cf5 taxonomy ../taxonomy\cf7 .\cf5 qza \\\
  \cf7 --\cf5 p\cf7 -\cf5 level \cf8 6\cf5 \\\
  \cf7 --\cf5 o\cf7 -\cf5 collapsed\cf7 -\cf5 table rarefied_table\cf7 -\cf5 l6\cf7 .\cf5 qza
\f0\fs27\fsmilli13600 \cf2 \cb3 \
\pard\tx560\tx1120\tx1680\tx2240\tx2800\tx3360\tx3920\tx4480\tx5040\tx5600\tx6160\tx6720\pardeftab720\pardirnatural\partightenfactor0
\cf2 \
\pard\pardeftab720\partightenfactor0

\f2\fs28 \cf5 \cb6 qiime tools export \\\
  --input-path rarefied_table-l6.qza \\\
  --output-path exported-feature-rarefied_table-l6
\f0\fs27\fsmilli13600 \cf2 \cb3 \
\pard\tx560\tx1120\tx1680\tx2240\tx2800\tx3360\tx3920\tx4480\tx5040\tx5600\tx6160\tx6720\pardeftab720\pardirnatural\partightenfactor0
\cf2 \
\pard\pardeftab720\partightenfactor0

\f2\fs28 \cf5 \cb6 biom convert -i feature-table.biom -o feature-rarefied_table-l6.tsv --to-tsv
\f0\fs27\fsmilli13600 \cf2 \cb3 \
}
