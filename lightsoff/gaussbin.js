function print(M, msg) {
  console.log("======" + msg + "=========")
  for(var i=0; i<M.length; ++i) {
    var str = "";
    for(var j=0; j<M.length; ++j) {
      str += M[i] & (1 << j) == 1 ? "1:" : "0:";
    }
    console.log(str);
  }
  console.log("==========================")
}

function diagonalize(M) {
  var m = M.length;
  for(var k=0; k<m; ++k) {
    // Find the k-th pivot
    i_max = findPivot(M, k);
    if (A[i_max, k] == 0)
      throw "matrix is singular";
    swap_rows(M, k, i_max);
    // Do for all rows below pivot
    for(var i=k+1; i<m; ++i) {
      // Do for all remaining elements in current row:
      // var c = A[i][k] / A[k][k];
      if (A[i] & (1 << k)) {
        A[i] ^= A[k];
      }
      //for(var j=k+1; j<n; ++j) {
      //  A[i][j] = A[i][j] - A[k][j];
      //}
      // Fill lower triangular matrix with zeros
      //A[i][k] = 0;
    }
  }
}

function findPivot(M, k) {
  for(var i=k; i<M.length; ++i) {
    if (M[i] & (1 << k) == 1)
      return i;
  }
  return k;
}

function swap_rows(M, i_max, k) {
  if (i_max != k) {
    var temp = A[i_max];
    A[i_max] = A[k];
    A[k] = temp;
  }
}

function makeM(A, b) {
  for(var i=0; i<A.length; ++i) {
    A[i] = (A[i] << 1) | b[i];
  }
}

function substitute(M) {
  var m = M.length;
  for(var i=m-1; i>=0; --i) {
    var x = M[i][m] / M[i][i];
    for(var j=i-1; j>=0; --j) {
      M[j][m] -= x * M[j][i];
      M[j][i] = 0;
    }
    M[i][m] = x;
    M[i][i] = 1;
  }
}

function extractX(M) {
  var x = [];
  var m = A.length;
  var n = A[0].length;
  for(var i=0; i<m; ++i){
    x.push(A[i][n-1]);
  }
  return x;
}

function solve(A, b) {
  print(A, " A ");
  makeM(A,b);
  print(A, " M ");
  debugger
  diagonalize(A);
  //print(A, "diag");
  substitute(A);
  //print(A, "subst");
  var x = extractX(A);
  //print(x, "x");
  return x;
}

function makeArow(m, ci, cj) {
  var A = 0;
  for(var i=0; i<m; ++i) {
    for(var j=0; j<m; ++j) {
      var di = Math.abs(i - ci);
      var dj = Math.abs(j - cj);
      var b = 0;
      if ((di == 1 && dj == 0) || (di == 0 && dj == 1) || (di == 0 && dj == 0)) {
        b = 1;
      }
      A = (A << 1) | b;
    }
  }
  return A;
}

function makeA(m) {
  var A = [];
  for(var i=0; i<m; ++i) {
    for(var j=0; j<m; ++j) {
      A.push(makeArow(m, i, j));
    }
  }
  return A;
}

// sample from: http://mathworld.wolfram.com/GaussianElimination.html


debugger

A = makeA(3);
b = [0,1,0,1,1,0,0,1,1];
print(A, " A ");

var x = solve(A, b);

print(x, " x ");
