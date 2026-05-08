FC = gfortran
FFLAGS = -O3 -march=native

SRCDIR = src
APPDIR = app
TESTDIR = test
BUILDDIR = build
EXEDIR = .

MOD_SRCS = $(wildcard $(SRCDIR)/*.f90)
MAIN_SRC = $(APPDIR)/main.f90
TEST_SRC = $(TESTDIR)/test_spline.f90

MOD_OBJS = $(patsubst $(SRCDIR)/%.f90,$(BUILDDIR)/%.o,$(MOD_SRCS))
MAIN_OBJ = $(BUILDDIR)/main.o
TEST_OBJ = $(BUILDDIR)/test_spline.o

MAIN_EXE = $(EXEDIR)/spline_interpolation
TEST_EXE = $(EXEDIR)/spline_test

all: $(MAIN_EXE) $(TEST_EXE)

$(BUILDDIR):
	mkdir -p $(BUILDDIR)

$(BUILDDIR)/%.o: $(SRCDIR)/%.f90 | $(BUILDDIR)
	$(FC) $(FFLAGS) -J$(BUILDDIR) -I$(BUILDDIR) -c $< -o $@

$(MAIN_OBJ): $(MAIN_SRC) | $(BUILDDIR)
	$(FC) $(FFLAGS) -J$(BUILDDIR) -I$(BUILDDIR) -c $< -o $@

$(TEST_OBJ): $(TEST_SRC) | $(BUILDDIR)
	$(FC) $(FFLAGS) -J$(BUILDDIR) -I$(BUILDDIR) -c $< -o $@

$(MAIN_EXE): $(MOD_OBJS) $(MAIN_OBJ)
	$(FC) $(FFLAGS) $^ -o $@

$(TEST_EXE): $(MOD_OBJS) $(TEST_OBJ)
	$(FC) $(FFLAGS) $^ -o $@

$(BUILDDIR)/tridiagonal_matrix.o: $(BUILDDIR)/precision_mod.o
$(BUILDDIR)/spline_interpolation.o : $(BUILDDIR)/tridiagonal_matrix.o


$(MAIN_OBJ): $(BUILDDIR)/precision_mod.o $(BUILDDIR)/tridiagonal_matrix.o $(BUILDDIR)/spline_interpolation.o
$(TEST_OBJ): $(BUILDDIR)/precision_mod.o $(BUILDDIR)/tridiagonal_matrix.o $(BUILDDIR)/spline_interpolation.o

.PHONY: all clean

clean:
	rm -rf $(BUILDDIR) $(MAIN_EXE) $(TEST_EXE)
