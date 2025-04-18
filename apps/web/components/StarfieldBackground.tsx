'use client'

import { useRef, useEffect } from 'react'
import { Canvas, useFrame } from '@react-three/fiber'
import { Points, PointMaterial } from '@react-three/drei'
import * as THREE from 'three'

// Generate random stars
function generateStars(count: number) {
  const positions = new Float32Array(count * 3)
  const colors = new Float32Array(count * 3)
  
  for (let i = 0; i < count; i++) {
    const i3 = i * 3
    positions[i3] = (Math.random() - 0.5) * 15
    positions[i3 + 1] = (Math.random() - 0.5) * 15
    positions[i3 + 2] = (Math.random() - 0.5) * 15
    
    colors[i3] = Math.random() * 0.3 + 0.7  // Mostly white with a bit of variation
    colors[i3 + 1] = Math.random() * 0.3 + 0.7
    colors[i3 + 2] = Math.random() * 0.3 + 0.7
  }
  
  return { positions, colors }
}

function Stars() {
  const ref = useRef<THREE.Points>(null!)
  const { positions, colors } = generateStars(3000)
  
  useFrame((state) => {
    if (ref.current) {
      ref.current.rotation.x = state.clock.getElapsedTime() * 0.05
      ref.current.rotation.y = state.clock.getElapsedTime() * 0.03
    }
  })
  
  return (
    <group>
      <Points ref={ref} positions={positions} stride={3} frustumCulled={false}>
        <PointMaterial
          transparent
          color="#ffffff"
          size={0.02}
          sizeAttenuation={true}
          depthWrite={false}
          vertexColors
        />
      </Points>
    </group>
  )
}

export default function StarfieldBackground() {
  return (
    <div className="space-bg">
      <Canvas camera={{ position: [0, 0, 3] }}>
        <color attach="background" args={['#000000']} />
        <ambientLight intensity={0.01} />
        <Stars />
      </Canvas>
    </div>
  )
} 