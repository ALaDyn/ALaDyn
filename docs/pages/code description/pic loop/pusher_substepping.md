title: Substepping

# Particle pusher substepping

In `ALaDyn` the currently available particle pushers are the standard [_Boris_](https://books.google.it/books?hl=en&lr=&id=dQRBAQAAIAAJ&oi=fnd&pg=PA3&dq=Relativistic+plasma+simulation-optimization+of+a+hybrid+code&ots=OTEPPhsXrf&sig=nMogaw16LP2bAGu1sitHuW8pOiY#v=onepage&q=Relativistic%20plasma%20simulation-optimization%20of%20a%20hybrid%20code&f=false) (BP) or the [_Higuera-Cary_](https://aip.scitation.org/doi/abs/10.1063/1.4979989) (HC). Each of them can be chosen in the input file by selecting

```fortran
pusher = 1
```

for the HC or

```fortran
pusher = 2
```

for the BP.

## Brief description of the algorithms

The usual implementation of the particle pusher divides the particle motion in four main steps, the first three concerning the momentum update and finally the position update:

1. Particles are accelerated for an half timestep using only the electric field \(\mathbf{E}\),
2. Then, they are rotated under the influence of the \(\mathbf{v} \times \mathbf{B}\) component of the Lorentz force
3. The second half timestep only retaining the \(\mathbf{E}\) field is performed.
4. Once the new momentum has been computed, the particle position is advanced via a standard Leap-frog timestep.

This algorithm, called the _Boris-Buneman_ (BB) rotation, is very powerful since it guarantees a second order accuracy that is consistent with the _Yee_ staggered Electromagnetic field integrator usually implemented in PIC schemes.
In addition, the _Boris-Buneman_ rotation preserves the particle energy when the particle rotates in the magnetic field. In fact, it computes the approximated rotation angle (up to \(\mathcal{O}(\Delta t^2)\)) and then it performes that rotation analitically.
However, laser pulses employed in LWFA schemes, can reach very high intensities, thus requiring particles to rotate for very large angles in a single timestep, as pointed out by [Arefiev](https://aip.scitation.org/doi/pdf/10.1063/1.4965624).

## Sub-cycling

The particle pusher sub-cycling may be a clever workaround to correct the particles trajectory when increasing the whole simulation resolution is not a viable option.

In `ALaDyn` via the substepping option in the input file

```fortran
n_substeps       = 1,
```

it is possible to subdivide the particle step to obtain a corrected final momentum.
Of course, the whole scheme precision will still be \(\mathcal{O}(\Delta t^2)\),
because the accuracy will be limited by the field solvers and the particle grid interaction,
but we can lower the error that can be accumulated due to an incorrect particle stepping.
The subcycling in `ALaDyn` maintains the same pusher structur as explained in the
previous section, repeating it \(N\) times with a timestep reduced as \(\Delta t/N\).

The particle substepping is implemented in the `boris_push` module as follows

```fortran
    do nss = 1, nstep
     !u^{-} = p_{n-1/2} + q*Lfact*(Ex,Ey,Bz)*Dt/2 in Boris push
     pp(1:np, 1) = pxp(1:np) + pt%aux1(1:np)*alp
     pp(1:np, 2) = pyp(1:np) + pt%aux2(1:np)*alp
     pp(1:np, 3) = pzp(1:np) + pt%aux3(1:np)*alp

     do p = 1, np
      b2(p) = dot_product(bb(p, 1:3), bb(p, 1:3))
      bv(p) = dot_product(bb(p, 1:3), pp(p, 1:3))
     end do

     !gam0 in Boris push gam0^2 = 1 + (u^-)^2
     do p = 1, np
      gam02(p) = 1. + dot_product(pp(p, 1:3), pp(p, 1:3))
     end do

     gam(1:np) = sqrt(gam02(1:np))

     !============================================
     ! p_n=(gam2*vp+gam*(vp crossb)+b*bv/(gam2+b2)
     !============================================

     ! New PX_COMP calculation
     aux(1:np) = gam02(1:np)*pp(1:np, 1) + bb(1:np, 1)*bv(1:np)
     aux(1:np) = (aux(1:np) + gam(1:np)*(pp(1:np, 2)*bb(1:np, 3) - &
      pp(1:np, 3)*bb(1:np, 2)))/(b2(1:np) + gam02(1:np))

     pxp(1:np) = 2.*aux(1:np) - pxp(1:np)

     ! New PY_COMP calculation
     aux(1:np) = gam02(1:np)*pp(1:np, 2) + bb(1:np, 2)*bv(1:np)
     aux(1:np) = (aux(1:np) + gam(1:np)*(pp(1:np, 3)*bb(1:np, 1) - &
      pp(1:np, 1)*bb(1:np, 3)))/(b2(1:np) + gam02(1:np))

     pyp(1:np) = 2.*aux(1:np) - pyp(1:np)

     ! New PZ_COMP calculation
     aux(1:np) = gam02(1:np)*pp(1:np, 3) + bb(1:np, 3)*bv(1:np)
     aux(1:np) = (aux(1:np) + gam(1:np)*(pp(1:np, 1)*bb(1:np, 2) - &
      pp(1:np, 2)*bb(1:np, 1)))/(b2(1:np) + gam02(1:np))

     pzp(1:np) = 2.*aux(1:np) - pzp(1:np)

    end do
```

where for every subcycle the `b2` vector caches \(|\mathbf{B}|^2\),
`bb` is the magnetic field \(\mathbf{B}\),
`bv` contains the scalar product \(\mathbf{B}\cdot \mathbf{u}^-\), with \(\mathbf{u}^-\) the updated momentum after the step 1. of the pusher scheme.

The `gam` and `gam02` vectors contain the Lorenz factor and the squared Lorentz factor respectively, where that could be defined as in the Boris push (see code)
$$\gamma_B^2=1+|\mathbf{u}^-|^2,$$
or as in the Higuera-Cary one
$$\gamma_H^2=\frac{1}{2}\left(\gamma_B^2 - \beta^2 + \sqrt{\left(\gamma_B^2 - \beta^2\right)^2+4\left(\beta^2+|\beta \cdot \mathbf{u}^-|\right)}\right),$$
where \(\beta=\gamma \theta/2 = q \mathbf{B}\Delta t/2mc\).

## Hints for the usage

Testing this feature we noted some features that might be of help
when choosing the input parameters"

- Pusher substepping works for both the available pushers (always check the results!)
- In general the HC converges much faster than the BP.
  This is particularly visible when you don't use any substepping.
- For the parameters that are usually in play, we noted that even for extreme high intensities
  (*i.e.* \(a_0 \geq 10\)), 5 substeps already converge to some result.
  Remember that in order to increase the overall accuracy, you will need to increase resolution
  considering that this technique doesn't solve other issues (*e.g.* interpolation errors,
  incorrect phase speed, etc...)
- We could't find a lot of improvement for the HC case, since it is almost converged already
  for `n_substeps = 1`.
