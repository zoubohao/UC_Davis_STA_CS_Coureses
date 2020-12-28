/*PROGRAM       : SAStesting.sas
 *PURPOSE       : a SAS program to test whether SAS UE can run properly. 
 */

* 1. Create a library, point to where you want to store datasets and results;

LIBNAME lab1 "/folders/myfolders/Lab1"; 
 

* 2.Import Data using proc import;

PROC IMPORT DATAFILE= "/folders/myfolders/Lab1/cows.csv"
	DBMS=CSV  /* Option DBMS specifies the type of data to import. In this case, the data type is CSV. 
			       */
	OUT=lab1.cow; /* Required Argument OUT identifies the output SAS data set with library and member name.
					   */
	GETNAMES=YES; /*Option GETNAMES determines whether to generate SAS variable names from the data values in the first 
					record of the imported file.*/
RUN;

* 3. Print SAS data set "LAB2.Test";
PROC PRINT DATA = lab1.cow; 
RUN; 

* 4. Print basic information of data, like data type, length, format;
PROC CONTENTS DATA=test; /* List the contents of a SAS data sets */
RUN;

* 5. Obtain Summary of Data;

/* To get the size, mean, standard deviation, median, minimum and maximum of data "Test" */  
PROC UNIVARIATE DATA=lab1.cow plots;
	 VAR prot;  
RUN;
