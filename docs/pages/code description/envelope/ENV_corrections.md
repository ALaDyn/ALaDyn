title: Corrected divergence

# Laser pulse divergence in the ponderomotive approximation

## Standard loop
The standard PIC loop associated with the ponderomotive approximation in `ALaDyn` is structured as

1. Compute the \(\Phi=|A|^2/2\) term associated with the Lorentz gamma factor,
2. Compute the gradient of \(\Phi\),
3. Interpolate \(\mathbf{E}\), \(\mathbf{B}\), \(\nabla\Phi\) and \(\Phi\) on particles position,
4. Update envelope momenta according to the [modified pusher](https://www.sciencedirect.com/science/article/pii/S0010465519301195),
$$\mathbf{p}^n = \mathbf{p}^{n-1/2}+\left(\widetilde{\mathbf{E}} - \widetilde{\Phi}/\gamma \right)+ \mathbf{p}^n/\gamma\times\widetilde{\mathbf{B}},$$
that is solved with a Boris like procedure. Lorentz factor is evaluated as
$$\widetilde{\gamma}^2 = \left[ \gamma_0^2 + 2\left(\widetilde{\mathbf{E}} - \widetilde{\Phi}/\gamma \right)\cdot\mathbf{p}^{n-1/2}\right], \quad \gamma_0^2=1+\Phi^n+|\mathbf{p}^{n-1/2}|^2.$$
5. Each particle \(W/\gamma\), where \(W\) is the weight, is deposited on the grid to get \(\left<n/\gamma \right>\),
6. Envelope field is advanced,
7. Again, \(\Phi\) and its gradient are computed, but this time the midtime \(A\) is used: \(A^{n+1/2} = \left(A^{n+1} - A^{n}\right)/2\),
8. The new \(\Phi\) and \(\nabla \Phi\) are interpolated on particles position,
9. Positions are advanced as
$$\mathbf{x}^{n+1} = \mathbf{x}^2+ \Delta t\frac{c\mathbf{p}^{n+1/2}}{\gamma^{n+1/2}},$$
where $$\gamma^{-1} = \frac{1}{\gamma_0}\left[ 1-\frac{\Delta t}{4\gamma_0^3}\left(\mathbf{p}^{n+1/2}\cdot \nabla \Phi\right)\right]$$.
10. From here on, the PIC loop is the standard one.


## Improved envelope

The introduction of the divergence correction on the envelope field inside the Lorentz factor, \(|\partial_x A|^2/2k_0^2\) does not alter the preiously described procedure. Instead, via the sostitution \(\Phi \to \Phi + \frac{1}{2k_0^2}|\partial_x A|^2\), one could reuse all the [1 - 7] points.

In `ALaDyn`, \(A\) is represented as \((A^R, A^I)\) in the complex space, so \(|\partial_x A|^2\) becomes \(\left(\partial_x A^R\right)^2 + \left(\partial_x A^I\right)^2\).

## Implementation

The loop associated with the system evolution in the ponderomotive approximation is the following

```fortran
  subroutine env_lpf2_evolve_new(it_loc, spec_in, spec_aux_in, mempool)
   integer, intent (in) :: it_loc
   type(species_new), allocatable, intent(inout), dimension(:) :: spec_in
   type(species_aux), allocatable, intent(inout), dimension(:) :: spec_aux_in
   type(memory_pool_t), pointer, intent(in) :: mempool
   integer :: np, ic
   integer, parameter :: sp_left = 2, sp_right = 2
   real (dp) :: ef2_ion, loc_ef2_ion(2)
   logical, parameter :: mw = .false.

   ic = 1
   !===========================
   call compute_ponderomotive_term(env, jc, oml, sp_left, sp_right)
   ! Computes |A|^2/2 or |A|^2/2 + 1/(2k_0^2)|dy A|^2
   call envelope_gradient(jc, sp_left, sp_right)
   !Exit staggered grad|A|^2/2 in jc(2:4) or jc(2:3) 
   !======================================
   !      jc(2:4)=grad|a|^2/2 at t^n
   ! For two-color |A|= |A_0|+|A_1|
   !======================================
   call set_env_acc(ebf, jc, spec_in(ic), spec_aux_in(ic), np, dt_loc, mempool)
   !=====================================
   !exit spec_aux_in(1:3)=q*[E+F] spec_aux_in(4:6)=q*B/gamp,
   !spec_aux_in(7)=wgh/gamp at t^n
   !Lorentz force already multiplied by particle charge
   !jc(1:4) not modified
   !====================
   call lpf_env_momenta(spec_in(ic), spec_aux_in(ic), np, ic, mempool)
   ! Updates particle momenta P^{n-1/2} => P^{n+1/2}
   ! stores in spec_aux_in(1:3)=old (x,y,z)^n spec_aux_in(7)=wgh/gamp >0
   !======================
   call set_env_density(spec_aux_in(ic), jc, np, 1, mempool)
   !==================================================
   ! in the envelope equation (A^{n-1},A^n)==> (A^n,A^{n+1})
   ! jc(3) = <q^2n/gam>
   ! Jc(1:2)=-ompe*jc(3)*A at level t^n
   !==================================================
   call advance_lpf_envelope(jc, env, oml)
   !advance (A^n, J^n) => A^{n+1}, A^{n-1}=> A^n
   ! jc(3) not modified
   !Boundary points of A are set to be 0 in env_bds
   !=======================
   call compute_ponderomotive_term_midtime(env, jc, oml, sp_left, sp_right)
   ! Same as compute_ponderomotive_term, but it first extract A^{n+1/2}
   ! In jc(1) stores the ponderomotive correction to the standard Lorentz gamma.
   ! The standard correction is jc(1)= Phi= |A|^2/2 +|A_1|/2 at t^{n+1/2}
   ! If the improved_ponderomotive flag is true, the correction is
   ! jc(1)= Phi= |A|^2/2 + |dy A|^2/2k0^2 at t^{n+1/2}
   ! To take into account the correct divergence
   call envelope_gradient(jc, sp_left, sp_right)
   !Exit staggered grad|A|^2/2 in jc(2:4) or jc(2:3) 
   call set_env_grad_interp(jc, spec_in(ic), spec_aux_in(ic), np, curr_ndim,
    mempool)
   !=============================
   ! Exit p-interpolated |A| field variables
   ! at time level t^{n+1/2} and positions at time t^n
   ! in spec_aux_in(1:3)=grad|A|^2/2 spec_aux_in(4)=|A|^2/2 in 3D
   ! in spec_aux_in(1:2)=grad|A|^2/2 spec_aux_in(3)=|A|^2/2 in 2D
   !=====================================
   call lpf_env_positions(spec_in(ic), spec_aux_in(ic), np, mempool)
   !===========================
   ! spec_aux_in(1:3) dt*V^{n+1/2}  spec_aux_in(4:6) old positions for curr J^{n+1/2}
   ! spec_aux_in(7)=dt*gam_inv
   call curr_accumulate(spec_in(ic), spec_aux_in(ic), jc, np, mempool)
   !===========================
   !====================
   ! Jc(1:3) for total curr Dt*J^{n+1/2}
   call advance_lpf_fields(ebf, jc, 0)
   ! (E,B) fields at time t^{n+1}
   !-----------------------------
  end subroutine
```
