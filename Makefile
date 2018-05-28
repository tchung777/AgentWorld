EXES := simworld scm_agent

all: $(EXES)

SIMWORLD_OBJS := agent.o        \
                 environment.o  \
                 log.o          \
                 main.o         \
                 vegetation.o

ifeq (Linux,$(shell uname))
LIBGUILE := /usr/lib64/libguile-2.0.so.22
LIBGMP := /scratch/COEN266/gmp-6.1.2/lib/libgmp.a
INCLUDE_GUILE := -I/scratch/COEN266/include
INCLUDE_GMP := -I/scratch/COEN266/gmp-6.1.2/include
else
LIBGMP :=
LIBGUILE := -L/usr/local/lib -lguile-2.0
INCLUDE_GMP :=
INCLUDE_GUILE := -I/usr/local/include/guile/2.0
endif

CFLAGS := -Wall -g -MD $(INCLUDE_GUILE) $(INCLUDE_GMP)
LDFLAGS := -g

simworld: $(SIMWORLD_OBJS)
	gcc $(LDFLAGS) $(SIMWORLD_OBJS) -o $@

%.o: %.c
	gcc $(CFLAGS) -c $< -o $@

SCM_AGENT_OBJS := scm_agent.o

scm_agent: $(SCM_AGENT_OBJS)
	gcc $(LDFLAGS) $(LIBGUILE) $(SCM_AGENT_OBJS) $(LIBGMP) -o $@

OBJS := $(SIMWORLD_OBJS) $(SCM_AGENT_OBJS)
DEPS := $(patsubst %.o,%.d,$(OBJS))

clean:
	rm -f $(OBJS) $(EXES) $(DEPS)

-include $(DEPS)

