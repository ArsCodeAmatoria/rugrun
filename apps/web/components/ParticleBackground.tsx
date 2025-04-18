'use client'

import { useEffect, useRef } from 'react'

interface Particle {
  x: number
  y: number
  size: number
  speedX: number
  speedY: number
  color: string
  alpha: number
}

export default function ParticleBackground() {
  const canvasRef = useRef<HTMLCanvasElement>(null)
  const particlesRef = useRef<Particle[]>([])
  const animationFrameRef = useRef<number>(0)
  
  useEffect(() => {
    const canvas = canvasRef.current
    if (!canvas) return
    
    const ctx = canvas.getContext('2d')
    if (!ctx) return
    
    const resizeCanvas = () => {
      canvas.width = window.innerWidth
      canvas.height = window.innerHeight
      
      // Recreate particles when canvas is resized
      createParticles()
    }
    
    const createParticles = () => {
      particlesRef.current = []
      const numParticles = Math.min(Math.floor(window.innerWidth * window.innerHeight / 15000), 100)
      
      for (let i = 0; i < numParticles; i++) {
        particlesRef.current.push({
          x: Math.random() * canvas.width,
          y: Math.random() * canvas.height,
          size: Math.random() * 2 + 0.5,
          speedX: (Math.random() - 0.5) * 0.3,
          speedY: (Math.random() - 0.5) * 0.3,
          color: getRandomColor(),
          alpha: Math.random() * 0.5 + 0.1
        })
      }
    }
    
    const getRandomColor = () => {
      const colors = [
        'rgba(147, 51, 234, 0.8)',  // purple
        'rgba(236, 72, 153, 0.8)',  // pink
        'rgba(6, 182, 212, 0.8)',   // cyan
        'rgba(16, 185, 129, 0.8)',  // emerald
      ]
      return colors[Math.floor(Math.random() * colors.length)]
    }
    
    const animate = () => {
      ctx.clearRect(0, 0, canvas.width, canvas.height)
      
      // Draw and update each particle
      particlesRef.current.forEach(particle => {
        ctx.globalAlpha = particle.alpha
        ctx.fillStyle = particle.color
        ctx.beginPath()
        ctx.arc(particle.x, particle.y, particle.size, 0, Math.PI * 2)
        ctx.fill()
        
        // Update position
        particle.x += particle.speedX
        particle.y += particle.speedY
        
        // Bounce off edges
        if (particle.x > canvas.width || particle.x < 0) {
          particle.speedX *= -1
        }
        
        if (particle.y > canvas.height || particle.y < 0) {
          particle.speedY *= -1
        }
      })
      
      // Draw connections between close particles
      drawConnections()
      
      animationFrameRef.current = requestAnimationFrame(animate)
    }
    
    const drawConnections = () => {
      const maxDistance = 150
      
      for (let i = 0; i < particlesRef.current.length; i++) {
        for (let j = i + 1; j < particlesRef.current.length; j++) {
          const p1 = particlesRef.current[i]
          const p2 = particlesRef.current[j]
          
          const dx = p1.x - p2.x
          const dy = p1.y - p2.y
          const distance = Math.sqrt(dx * dx + dy * dy)
          
          if (distance < maxDistance) {
            // Set line opacity based on distance
            const alpha = (1 - distance / maxDistance) * 0.2
            
            ctx.globalAlpha = alpha
            ctx.strokeStyle = p1.color
            ctx.lineWidth = 0.5
            ctx.beginPath()
            ctx.moveTo(p1.x, p1.y)
            ctx.lineTo(p2.x, p2.y)
            ctx.stroke()
          }
        }
      }
    }
    
    // Initialize
    resizeCanvas()
    animate()
    
    // Event listeners
    window.addEventListener('resize', resizeCanvas)
    
    // Cleanup
    return () => {
      window.removeEventListener('resize', resizeCanvas)
      cancelAnimationFrame(animationFrameRef.current)
    }
  }, [])
  
  return (
    <canvas 
      ref={canvasRef}
      className="fixed top-0 left-0 w-full h-full -z-10 bg-black"
    />
  )
} 