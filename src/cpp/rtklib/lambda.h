
#ifndef __LAMBDA_H_
#define __LAMBDA_H_

/* integer ambiguity resolution ----------------------------------------------*/
int lambda(
	Trace& trace, 
	int n,
	int m,
	const double *a, 
	const double *Q,
	double *F, 
	double *s,
	double Pf,
	bool& pass);

int newLambda(
	Trace&			trace,			
	int				numInts,		
	int				numSols,		
	VectorXd&		floatSol,
	MatrixXd&		QMat,		
	MatrixXd&		ZMat,			
	double*			F,				
	double*			solResiduals,	
	double			Pf,				
	int*			index);	

#endif
