// This is the main DLL file.

#include "stdafx.h"

#include "ClassLibrary1.h"

#include <Test.h>

double ClassLibrary1::Class1::InverseIncompleteBeta(double a, double b, double x)
{
	return Test::InverseIncompleteBeta(a,b,x);
}

double ClassLibrary1::Class1::NormalDistribution(double m, double s, double quantile)
{
	return Test::NormalDistribution(m,s,quantile);
}
