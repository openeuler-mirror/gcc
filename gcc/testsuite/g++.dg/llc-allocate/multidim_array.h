#ifndef MULTIDIM_ARRAY_H
#define MULTIDIM_ARRAY_H

#include <iostream>

#define RELION_ALIGNED_MALLOC malloc
#define RELION_ALIGNED_FREE free

#define STARTINGX(v) ((v).xinit)
#define STARTINGY(v) ((v).yinit)
#define NZYXSIZE(v) ((v).nzyxdim)

#define DIRECT_MULTIDIM_ELEM(v,n) ((v).data[(n)])
#define FOR_ALL_DIRECT_ELEMENTS_IN_MULTIDIMARRAY(v) \
  for (long int n=0; n<NZYXSIZE(v); ++n)

#define FOR_ALL_DIRECT_ELEMENTS_IN_MULTIDIMARRAY_ptr(v,n,ptr) \
  for ((n)=0, (ptr)=(v).data; (n)<NZYXSIZE(v); ++(n), ++(ptr))

#define DIRECT_A2D_ELEM(v,i,j) ((v).data[(i)*(v).xdim+(j)])
#define A2D_ELEM(v, i, j) \
  DIRECT_A2D_ELEM(v, (i) - STARTINGY(v), (j) - STARTINGX(v))

#define DIRECT_A1D_ELEM(v, i) ((v).data[(i)])
#define A1D_ELEM(v, i) DIRECT_A1D_ELEM(v, (i) - ((v).xinit))

template<typename T>
class MultidimArray
{
public:
  T* data;
  bool destroyData;
  long int ndim;
  long int zdim;
  long int ydim;
  long int xdim;
  long int yxdim;
  long int zyxdim;
  long int nzyxdim;
  long int zinit;
  long int yinit;
  long int xinit;
  long int nzyxdimAlloc;

public:
  void clear()
  {
    coreDeallocate();
    coreInit();
  }

  void coreInit()
  {
    xdim=0;
    yxdim=0;
    zyxdim=0;
    nzyxdim=0;
    ydim=1;
    zdim=1;
    ndim=1;
    zinit=0;
    yinit=0;
    xinit=0;
    data=NULL;
    nzyxdimAlloc = 0;
    destroyData=true;
  }

  void coreAllocate(long int _ndim, long int _zdim, long int _ydim, long int _xdim)
  {
    if (_ndim <= 0 || _zdim <= 0 || _ydim<=0 || _xdim<=0)
      {
        clear();
        return;
      }

    ndim=_ndim;
    zdim=_zdim;
    ydim=_ydim;
    xdim=_xdim;
    yxdim=ydim*xdim;
    zyxdim=zdim*yxdim;
    nzyxdim=ndim*zyxdim;

    coreAllocate();
  }

  void coreAllocate()
  {
    data = (T*)RELION_ALIGNED_MALLOC(sizeof(T) * nzyxdim);
    nzyxdimAlloc = nzyxdim;
  }

  void coreDeallocate()
  {
    if (data != NULL && destroyData)
      {
        RELION_ALIGNED_FREE(data);
      }
    data=NULL;
    nzyxdimAlloc = 0;
  }

  void resize(long int Ndim, long int Zdim, long int Ydim, long int Xdim)
  {
    if (Ndim*Zdim*Ydim*Xdim == nzyxdimAlloc && data != NULL)
      {
        ndim = Ndim;
        xdim = Xdim;
        ydim = Ydim;
        zdim = Zdim;
        yxdim = Ydim * Xdim;
        zyxdim = Zdim * yxdim;
        nzyxdim = Ndim * zyxdim;
        nzyxdimAlloc = nzyxdim;
        return;
      }

    if (Xdim <= 0 || Ydim <= 0 || Zdim <= 0 || Ndim <= 0)
      {
        clear();
        return;
      }

    if (NZYXSIZE(*this) > 0 && data == NULL)
      {
        coreAllocate();
        return;
      }

    size_t YXdim=Ydim*Xdim;
    size_t ZYXdim=Zdim*YXdim;
    size_t NZYXdim=Ndim*ZYXdim;

    T * new_data = (T*)RELION_ALIGNED_MALLOC(sizeof(T) * NZYXdim);
    for (long int l = 0; l < Ndim; l++)
        for (long int k = 0; k < Zdim; k++)
            for (long int i = 0; i < Ydim; i++)
                for (long int j = 0; j < Xdim; j++)
                  {
                    T val;
                    new_data[l*ZYXdim + k*YXdim+i*Xdim+j] = val;
                  }
    coreDeallocate();

    data = new_data;
    ndim = Ndim;
    xdim = Xdim;
    ydim = Ydim;
    zdim = Zdim;
    yxdim = Ydim * Xdim;
    zyxdim = Zdim * yxdim;
    nzyxdim = Ndim * zyxdim;
    nzyxdimAlloc = nzyxdim;
  }

  void resize(long int Xdim)
  {
    resize(1, 1, 1, Xdim);
  }

  inline T& operator()(long int i, long int j) const
  {
    return A2D_ELEM(*this, i, j);
  }

  inline T& operator()(long int i) const
  {
    return A1D_ELEM(*this, i);
  }

  void getRow(long int i, MultidimArray<T>& v) const
  {
    if (xdim == 0 || ydim == 0)
      {
        v.clear();
        return;
      }

    v.resize(xdim);
    for (long int j = 0; j < xdim; j++)
      v(j) = (*this)(i, j);
  }
};

#endif /* MULTIDIM_ARRAY_H */