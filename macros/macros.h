#define MAC_VECTOR3_NORM(v)     sqrt(v(1)*v(1) + v(2)*v(2) + v(3)*v(3))

#define MAC_MATRIX33_TRACE(_M)       ( _M(1,1)+_M(2,2)+_M(3,3) )

#define MAC_MATRIX33_DET(_M)         (                                              \
                                 _M(1,1)*(_M(2,2)*_M(3,3) - _M(3,2)*_M(2,3)) \
                               - _M(1,2)*(_M(2,1)*_M(3,3) - _M(3,1)*_M(2,3)) \
                               + _M(1,3)*(_M(2,1)*_M(3,2) - _M(3,1)*_M(2,2)) \
                              )

#define MAC_MATRIX33_TRANSPOSE(_M,_MT)  \
   _MT(1,1) = _M(1,1); \
   _MT(1,2) = _M(2,1); \
   _MT(1,3) = _M(3,1); \
   _MT(2,1) = _M(1,2); \
   _MT(2,2) = _M(2,2); \
   _MT(2,3) = _M(3,2); \
   _MT(3,1) = _M(1,3); \
   _MT(3,2) = _M(2,3); \
   _MT(3,3) = _M(3,3)

#define MAC_MATRIX33_PROD(_ML,_MR,_MM)  \
   _MM(1,1) = _ML(1,1)*_MR(1,1) + _ML(1,2)*_MR(2,1) + _ML(1,3)*_MR(3,1); \
   _MM(1,2) = _ML(1,1)*_MR(1,2) + _ML(1,2)*_MR(2,2) + _ML(1,3)*_MR(3,2); \
   _MM(1,3) = _ML(1,1)*_MR(1,3) + _ML(1,2)*_MR(2,3) + _ML(1,3)*_MR(3,3); \
   _MM(2,1) = _ML(2,1)*_MR(1,1) + _ML(2,2)*_MR(2,1) + _ML(2,3)*_MR(3,1); \
   _MM(2,2) = _ML(2,1)*_MR(1,2) + _ML(2,2)*_MR(2,2) + _ML(2,3)*_MR(3,2); \
   _MM(2,3) = _ML(2,1)*_MR(1,3) + _ML(2,2)*_MR(2,3) + _ML(2,3)*_MR(3,3); \
   _MM(3,1) = _ML(3,1)*_MR(1,1) + _ML(3,2)*_MR(2,1) + _ML(3,3)*_MR(3,1); \
   _MM(3,2) = _ML(3,1)*_MR(1,2) + _ML(3,2)*_MR(2,2) + _ML(3,3)*_MR(3,2); \
   _MM(3,3) = _ML(3,1)*_MR(1,3) + _ML(3,2)*_MR(2,3) + _ML(3,3)*_MR(3,3)
