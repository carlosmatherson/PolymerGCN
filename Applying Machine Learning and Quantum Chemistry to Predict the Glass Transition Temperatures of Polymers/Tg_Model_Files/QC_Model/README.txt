Data directory

The Data directory consists of the data necessary to run the model. "Measure_Tg_QC.xlsx" consists of measured Tg values as well as the calculated and compiled molecular properties which are the independent variables in the stepwise regression model.

The G16_Files directory consists of the input job (.com) and output log (.log) files for the electronic structure calculations run on Gaussian 16. File names consist of the polymer name followed by a label for the number of repeating units for the structure (1M, 2M and 3M for monomer, dimer and trimer respectively). The job files are set up to run three gas phase geometry optimizations with increasing basis set size followed by a population analysis for calculation of partial charge, a volume calculation and SMD solvation calculations for both water and octanol for calculation of the octanol-water partition coefficient (log Kow).

Scripts directory

Log_Read_01_23.R: read the .log files, extract the pertinent information and save to a .csv file.

Read_Computation_Time.R: Extract the cpu hours for the electronic structure calculations.

Tg_QC_Results.R: Read in the measured Tg and independent variables, run the stepwise regression model and plot the results.

