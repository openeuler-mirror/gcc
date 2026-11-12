struct Base
{
  virtual unsigned read (unsigned) = 0;
};
extern "C" unsigned outside (Base *);
