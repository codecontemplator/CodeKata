// ClassLibrary1.h

#pragma once

using namespace System;

namespace ClassLibrary1 {

	public ref class Class1
	{
	public:

		double static InverseIncompleteBeta(double a, double b, double x);
		double static NormalDistribution(double m, double s, double quantile);
	};
}
